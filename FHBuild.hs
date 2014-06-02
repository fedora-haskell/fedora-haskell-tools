-- |
-- Module      :  Main
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for building RPM packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

import Control.Applicative ((<$>))
import Control.Monad (filterM, unless, when)
import Data.Maybe (fromMaybe, isJust)
import Data.List (intercalate, isPrefixOf, stripPrefix)

import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), takeBaseName, takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess, readProcessWithExitCode, rawSystem)

data BuildMode = Local | Mock | Koji deriving (Eq)

main :: IO ()
main = do
  (com:dist:pkgs, mdir) <- getArgs >>= parseArgs
  mapM_ (build (mode com) dist mdir) pkgs
  where
    mode "local" = Local
    mode "mock" = Mock
    mode "koji" = Koji
    mode _ = undefined

commands :: [String]
commands = ["local", "mock" , "koji"]

dists :: [String]
dists = ["f21", "f20", "f19"]

-- allow "fhbuild CMD" or "fhbuild CMD DIST PKG..."
parseArgs :: [String] -> IO ([String], Maybe FilePath)
parseArgs [c] | c `elem` commands = do
  (dist:pkgs, dir) <- determinePkgBranch
  return (c:dist:pkgs, Just dir)
parseArgs (c:dist:pkgs) |  c `elem` commands
                           && dist `elem` dists
                           && not (null pkgs) =
                             return (c:dist:pkgs, Nothing)
parseArgs _ = help >> return ([], Nothing)

determinePkgBranch :: IO ([String], FilePath) -- (branch:pkgs, dir)
determinePkgBranch = do
  dir <- getCurrentDirectory
  let base = takeBaseName dir
  if base `elem` ["master", "f20", "f19"]
    then return ([base, takeBaseName $ takeDirectory dir], dir)
    else do
    git <- doesDirectoryExist (dir </> ".git")
    if git
      then do
      branch <- gitBranch
      return ([branch, base], dir)
      else
      error "Not a git repo: cannot determine branch"

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "CMD [dist pkg ...]\n"
    ++ "\n"
    ++ "Commands:\n"
    ++ "  local\t\t- build locally\n"
    ++ "  mock\t\t- build in mock\n"
    ++ "  koji\t\t- build in Koji\n"
  exitWith (ExitFailure 1)

dist2branch :: String -> String
dist2branch "f21" = "master"
dist2branch d = d

cmd :: String -> [String] -> IO String
cmd c as = readProcess c as ""

singleLine :: String -> String
singleLine = head . lines

cmdMaybe :: String -> [String] -> IO (Maybe String)
cmdMaybe c as = do
  (ret, out, _err) <- readProcessWithExitCode c as ""
  case ret of
    ExitSuccess -> return $ Just out
    ExitFailure _ -> return Nothing

cmd_ :: String -> [String] -> IO ()
cmd_ c as = do
  ret <- rawSystem c as
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ show n

cmdAssert :: String -> String -> [String] -> IO ()
cmdAssert msg c as = do
  ret <- rawSystem c as
  case ret of
    ExitSuccess -> return ()
    ExitFailure _ -> error msg

cmdlog :: String -> [String] -> IO ()
cmdlog c as = do
  date <- singleLine <$> cmd "date" ["+%T"]
  putStrLn $ date +-+ c +-+ unwords as
  cmd_ c as

cmdBool :: String -> [String] -> IO Bool
cmdBool c as = do
  ret <- rawSystem c as
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

sudo :: String -> [String] -> IO ()
sudo c as = cmdlog "sudo" (c:as)

shell :: String -> IO String
shell c = cmd "sh" ["-c", c]

(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

build :: BuildMode -> String -> Maybe FilePath -> String -> IO ()
build mode dist mdir pkg = do
  let dir = fromMaybe pkg mdir
      branch = dist2branch dist
  d <- if isJust mdir then return True else doesDirectoryExist dir
  putStrLn $ "==" +-+ pkg ++ ":" ++ dist2branch dist +-+ "=="
  unless d $ do
    let anon = ["-a" | mode /= Koji]
    cmd_ "fedpkg" $ ["clone", "-b", branch, pkg] ++ anon
  b <- doesDirectoryExist $ dir </> branch
  let wd = dir </> if b then branch else ""
  cmdAssert "not a Fedora pkg git dir!" "grep" ["-q", "pkgs.fedoraproject.org", wd </> ".git/config"]
  when d $
    -- FIXME check correct branch
    cmd_ "git" ["-C", wd, "pull"]
  nvr <- singleLine <$> cmd "fedpkg" ["--path", wd, "verrel"]
  let verrel = removePrefix (pkg ++ "-") nvr
  case mode of
    Local -> do
      installed <- fmap singleLine <$> cmdMaybe "rpm" ["-q", "--qf", "%{name}-%{version}-%{release}", pkg]
      if Just nvr == installed
        then putStrLn $ nvr +-+ "already installed!"
        else do
        putStrLn $ fromMaybe "Not installed" installed +-+ "->" +-+ nvr
        cmd_ "git" ["--no-pager", "-C", wd, "log", "-1"]
        putStrLn ""
        deps <- lines <$> cmd "rpmspec" ["-q", "--buildrequires", wd </> pkg ++ ".spec"]
        missing <- filterM notInstalled deps
        let hmissing = filter (isPrefixOf "ghc-") missing
        unless (null hmissing) $ do
          putStrLn $ "Missing:" +-+ intercalate ", " hmissing
          mapM_ (fhbuildMissing dist) hmissing
        stillMissing <- filterM notInstalled missing
        unless (null stillMissing) $ do
          putStrLn $ "Missing:" +-+ intercalate ", " stillMissing
          sudo "yum-builddep" ["-y", wd </> pkg ++ ".spec"]
        putStrLn $ "Building" +-+ nvr +-+ "(see" +-+ wd </> ".build-" ++ verrel ++ ".log" ++ ")"
        cmdlog "fedpkg" ["--path", wd, "local"]
--        -- FIXME: drop this?
--        sudo "yum" ["remove", pkg]
        arch <- singleLine <$> cmd "arch" []
        rpms <- lines <$> shell ("ls" +-+ wd </> arch </> "*-" ++ verrel ++ "." ++ arch ++ "." ++ "rpm")
        sudo "yum" $ ["-y", "localinstall"] ++ rpms
    Mock -> do
      putStrLn $ "Mock building" +-+ nvr
      cmdlog "fedpkg" ["--path", wd, "mockbuild"]
    Koji -> do
      cmd_ "git" ["--no-pager", "-C", wd, "log", "-1"]
      let target = dist ++ "-build"
      -- FIXME: handle case of no build
      latest <- (head . words . singleLine) <$> cmd "koji" ["latest-pkg", "--quiet", target, pkg]
      if nvr == latest
        then error $ nvr +-+ "already built!"
        else do
        putStrLn $ latest +-+ "->" +-+ nvr
        cmdlog "fedpkg" ["--path", wd, "build"]
        when (dist /= "f21") $ do
          user <- singleLine <$> shell "grep Subject: ~/.fedora.cert | sed -e 's@.*CN=\\(.*\\)/emailAddress=.*@\\1@'"
          -- FIXME: improve Notes with recursive info
          cmdlog "bodhi" ["-o", nvr, "-u", user, "-N", pkg +-+ "stack"]
        cmdlog "koji" ["wait-repo", target, "--build=" ++ nvr]

notInstalled :: String -> IO Bool
notInstalled dep = do
  pkg <- singleLine <$> cmd "repoquery" ["--qf", "%{name}", "--whatprovides", dep]
  not <$> cmdBool "rpm" ["--quiet", "-q", pkg]

fhbuildMissing :: String -> String -> IO ()
fhbuildMissing dist dep = do
  base <- singleLine <$> cmd "repoquery" ["--qf", "%{base_package_name}", "--whatprovides", dep]
  build Local dist Nothing base

removePrefix :: String -> String -> String
removePrefix prefix orig =
  fromMaybe (error prefix +-+ "is not prefix of" +-+ orig) $ stripPrefix prefix orig

removeSuffix :: String -> String -> String
removeSuffix suffix orig =
  fromMaybe orig $ stripSuffix suffix orig
  where
    stripSuffix sf str = reverse <$> stripPrefix (reverse sf) (reverse str)

gitBranch :: IO String
gitBranch =
  (removePrefix "* " . singleLine) <$> cmd "git" ["branch"]

