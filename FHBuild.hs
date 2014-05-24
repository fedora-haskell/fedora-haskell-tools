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

import HSH

import Control.Monad (when, unless)
import Data.Maybe (fromMaybe, isJust)
import Data.List (stripPrefix)
import Options.Applicative
import System.Directory (doesDirectoryExist)
import System.Environment (getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), takeBaseName, takeDirectory)
import System.IO (hPutStrLn, stderr)

type Dist = String -- TODO: This should really be a sum type instead.
type Pkgs = [String]

data BuildMode = Local Dist (Maybe FilePath) Pkgs
               | Mock Dist (Maybe FilePath) Pkgs
               | Koji Dist (Maybe FilePath) Pkgs

genOptions :: (Dist -> Maybe FilePath -> Pkgs -> BuildMode) -> Parser BuildMode
genOptions x =
  x <$> strOption (long "dist"
                   <> short 'd'
                   <> metavar "DIST"
                   <> help "Fedora/RHEL version to build for, e.g. f20 or EL-6")
    <*> option (long "directory"
                   <> short 'o'
                   <> metavar "DIRECTORY"
                   <> help "The working directory to house the build")
    <*> many (argument str (metavar "PKGS"))

localOptions :: Parser BuildMode
localOptions = genOptions Local

mockOptions :: Parser BuildMode
mockOptions = genOptions Mock

kojiOptions :: Parser BuildMode
kojiOptions = genOptions Koji

parseOptions :: Parser BuildMode
parseOptions = subparser
               (command "local" (info localOptions (
                                    progDesc "Build locally"))
                <> command "mock" (info mockOptions (
                                    progDesc "Build in mock"))
                <> command "koji" (info kojiOptions (
                                    progDesc "Build in koji"))
               )

opts :: ParserInfo BuildMode
opts = info (parseOptions <**> helper) idm

main :: IO ()
main = execParser opts >>= runBuild

runBuild :: BuildMode -> IO ()
runBuild mode@(Local dist mdir pkgs) = mapM_ (build mode dist mdir) pkgs
runBuild mode@(Mock dist mdir pkgs) = mapM_ (build mode dist mdir) pkgs
runBuild mode@(Koji dist mdir pkgs) = mapM_ (build mode dist mdir) pkgs

commands :: [String]
commands = ["local", "mock" {-, "koji"-}]

parseArgs :: [String] -> IO [String]
parseArgs args | (length args == 1 || length args >= 3)
                 && head args `elem` commands =
  return args
parseArgs _ = help' >> return []

determinePkgBranch :: IO ([String], Maybe FilePath) -- (branch:pkgs, dir)
determinePkgBranch = do
  dir <- pwd
  let base = takeBaseName dir
  if base `elem` ["master", "f20", "f19"]
    then return ([base, takeBaseName $ takeDirectory dir], Just dir)
    else do
    git <- doesDirectoryExist (dir </> ".git")
    if git
      then do
      branch <- gitBranch
      return ([branch, base], Just dir)
      else
      error "Not a git repo: cannot determine branch"

help' :: IO ()
help' = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "CMD [dist pkg ...]\n"
    ++ "\n"
    ++ "Commands:\n"
    ++ "  local\t\t- build locally\n"
    ++ "  mock\t\t- build in mock\n"
--    ++ "  koji\t\t- build in Koji\n"
  exitWith (ExitFailure 1)

dist2branch :: String -> String
dist2branch "f21" = "master"
dist2branch d = d

cmd :: String -> [String] -> IO String
cmd c as = run (c, as)

cmd_ :: String -> [String] -> IO ()
cmd_ c as = runIO (c, as)

cmdput :: String -> [String] -> IO ()
cmdput c as = do
  putStrLn $ "Running:" +-+ c +-+ unwords as
  runIO (c, as)

cmdSL :: String -> [String] -> IO String
cmdSL c as = runSL (c, as)

sudo :: String -> [String] -> IO ()
sudo c as = cmdput "sudo" (c:as)

(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

build :: BuildMode -> String -> Maybe FilePath -> String -> IO ()
build mode dist mdir pkg = do
  let dir = fromMaybe pkg mdir
      branch = dist2branch dist
  d <- if isJust mdir then return True else doesDirectoryExist dir
  unless d $
    cmd_ "fedpkg" ["clone", "-b", branch, pkg]
  b <- doesDirectoryExist $ dir </> branch
  putStrLn $ "==" +-+ pkg ++ ":" ++ dist2branch dist +-+ "=="
  let wd = dir </> if b then branch else ""
  when d $
    -- FIXME check correct branch
    cmd_ "git" ["-C", wd, "pull"]
  nvr <- cmdSL "fedpkg" ["--path", wd, "verrel"]
  let verrel = removePrefix (pkg ++ "-") nvr
  case mode of
    Local _ _ _ -> do
      installed <- cmd "rpm" ["-q", "--qf", "%{name}-%{version}-%{release}", pkg]
      if nvr == installed
        then putStrLn $ nvr +-+ "already installed!"
        else do
        putStrLn $ installed +-+ "->" +-+ nvr
        cmd_ "git" ["-C", wd, "log", "-2"]
        -- FIXME: only if missing deps
        sudo "yum-builddep" ["-y", wd </> pkg ++ ".spec"]
        putStrLn $ "Building" +-+ nvr +-+ "(see" +-+ wd </> ".build-" ++ verrel ++ ".log" ++ ")"
        cmdput "fedpkg" ["--path", wd, "local"]
        sudo "yum" ["remove", pkg]
        arch <- run "arch"
        rpms <- glob $ wd </> arch </> "*-" ++ verrel ++ "." ++ arch ++ "." ++ "rpm"
        sudo "yum" $ "localinstall":rpms
    Mock _ _ _ -> do
      putStrLn $ "Mock building" +-+ nvr
      cmdput "fedpkg" ["--path", wd, "mockbuild"]
    Koji _ _ _ -> putStrLn "FIXME"

removePrefix :: String -> String -> String
removePrefix prefix orig =
  fromMaybe (error prefix +-+ "is not prefix of" +-+ orig) $ stripPrefix prefix orig

gitBranch :: IO String
gitBranch = do
  out <- runSL ("git", ["branch"])
  return $ removePrefix "* " out

