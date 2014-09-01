-- |
-- Module      :  Build
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: Main entry point for building RPM packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (filterM, unless, when)
import Data.Maybe (fromMaybe, isJust)
import Data.List (intercalate, isPrefixOf, nub)

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), dropExtension, takeBaseName, takeDirectory)
import System.IO (hPutStrLn, stderr)

import Dists
import Utils

data Command = Install | Mock | Koji | Pending | Changed deriving (Eq)

main :: IO ()
main = do
  (com:dist:pkgs, mdir) <- getArgs >>= parseArgs
  mapM_ (build (mode com) dist mdir Nothing) pkgs
  where
    mode "install" = Install
    mode "mock" = Mock
    mode "koji" = Koji
    mode "pending" = Pending
    mode "changed" = Changed
    mode _ = undefined

commands :: [String]
commands = ["install", "mock" , "koji", "pending", "changed"]

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "CMD [dist] [pkg] ...\n"
    ++ "\n"
    ++ "Commands:\n"
    ++ "  install\t- build locally and install\n"
    ++ "  mock\t\t- build in mock\n"
    ++ "  koji\t\t- build in Koji\n"
    ++ "  pending\t- show planned changes\n"
    ++ "  changed\t- show changed pkgs\n"
  exitWith (ExitFailure 1)

-- allow "fhbuild CMD", "fhbuild CMD dir" or "fhbuild CMD DIST PKG..."
parseArgs :: [String] -> IO ([String], Maybe FilePath)
parseArgs [c] | c `elem` commands = do
  (dist:pkgs, dir) <- determinePkgBranch
  return (c:dist:pkgs, Just dir)
parseArgs [c, pkg] | c `elem` commands = do
  cwd <- getCurrentDirectory
  exists <- doesDirectoryExist pkg
  if exists then do
    setCurrentDirectory pkg
    (dist:pkgs, dir) <- determinePkgBranch
    setCurrentDirectory cwd
    return (c:dist:pkgs, Just dir)
    else return ([c, rawhide, pkg], Nothing)
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

dist2branch :: String -> String
dist2branch d | d == rawhide = "master"
              | otherwise = d

build :: Command -> String -> Maybe FilePath -> Maybe String -> String -> IO ()
build mode dist mdir mdep pkg = do
  let dir = fromMaybe pkg mdir
      branch = dist2branch dist
  dirExists <- if isJust mdir then return True else doesDirectoryExist dir
  unless (mode `elem` [Pending, Changed]) $
    putStrLn $ "\n==" +-+ pkg ++ ":" ++ dist2branch dist +-+ "=="
  unless dirExists $ do
    let anon = ["-a" | mode /= Koji]
    cmdlog "fedpkg" $ ["clone", "-b", branch, pkg] ++ anon
  b <- doesDirectoryExist $ dir </> branch
  let wd = dir </> if b then branch else ""
  retired <- doesFileExist $ wd </> "dead.package"
  unless retired $ do
    cmdAssert "not a Fedora pkg git dir!" "grep" ["-q", "pkgs.fedoraproject.org", wd </> ".git/config"]
    when dirExists $
      -- FIXME check correct branch
      cmd_ "git" ["-C", wd, "pull", "-q"]
    nvr <- cmd "fedpkg" ["--path", wd, "verrel"]
    let verrel = removePrefix (pkg ++ "-") nvr
    case mode of
      Install -> do
        let req = fromMaybe pkg mdep
        installed <- cmdMaybe "rpm" ["-q", "--qf", "%{name}-%{version}-%{release}", req]
        if Just (req ++ "-" ++ verrel) == installed
          then putStrLn $ nvr +-+ "already installed!\n"
          else do
          putStrLn $ fromMaybe "Not installed" installed +-+ "->" +-+ nvr
          cmd_ "git" ["--no-pager", "-C", wd, "log", "-1"]
          putStrLn ""
          let spec = wd </> pkg ++ ".spec"
          putStrLn "repoquerying deps..."
          -- "pkg = X.Y" -> ["pkg", "=", "X.Y"] -> "pkg"
          depvers <- (map (processDeps . words) . lines) <$> cmd "rpmspec" ["-q", "--buildrequires", spec] >>= mapM derefPkg
          missing <- nub <$> filterM notInstalled depvers
          -- FIXME sort into build order
          let hmissing = filter (\ dp -> "ghc-" `isPrefixOf` dp || dp `elem` ["alex", "cabal-install", "gtk2hs-buildtools", "happy"]) (map fst missing)
          unless (null hmissing) $ do
            putStrLn "Missing:"
            mapM_ putStrLn hmissing
            mapM_ (fhbuildMissing dist) hmissing
          stillMissing <- map (uncurry maybePkgVer) <$> filterM notInstalled missing
          unless (null stillMissing) $ do
            putStrLn $ "Installing:" +-+ intercalate ", " stillMissing
            sudo "yum" $ ["install", "-y"] ++ stillMissing
          putStrLn $ "Building" +-+ nvr +-+ "(buildlog:" +-+ wd </> ".build-" ++ verrel ++ ".log" ++ ")"
          -- workaround "fedpkg --path dir local" saving .build.log in cwd
          withCurrentDirectory wd $
            cmdlog "fedpkg" ["-q", "local"]
          putStrLn $ nvr +-+ "built\n"
          opkgs <- lines <$> cmd "rpmspec" ["-q", "--queryformat", "%{name}\n", spec]
          ipkgs <- lines <$> cmd "rpm" ("-qa":opkgs)
          unless (null ipkgs) $
            sudo "yum" ("remove":ipkgs)
          arch <- cmd "arch" []
          rpms <- (map (\ p -> wd </> arch </> p ++ ".rpm") . lines) <$>
                  cmd "rpmspec" ["-q", spec]
          sudo "yum" $ ["-y", "localinstall"] ++ rpms
      Mock -> do
        putStrLn $ "Mock building" +-+ nvr
        cmdlog "fedpkg" ["--path", wd, "mockbuild"]
      Koji -> do
        cmd_ "git" ["--no-pager", "-C", wd, "log", "-1"]
        let target = dist ++ "-build"
        -- FIXME: handle case of no build
        latest <- kojiLatestPkg target pkg
        if nvr == latest
          then error $ nvr +-+ "already built!"
          else do
          putStrLn $ latest +-+ "->" +-+ nvr
          cmdlog "fedpkg" ["--path", wd, "build"]
          when (dist /= rawhide) $ do
            user <- shell "grep Subject: ~/.fedora.cert | sed -e 's@.*CN=\\(.*\\)/emailAddress=.*@\\1@'"
            -- FIXME: improve Notes with recursive info
            cmdlog "bodhi" ["-o", nvr, "-u", user, "-N", pkg +-+ "stack"]
          cmdlog "koji" ["wait-repo", target, "--build=" ++ nvr]
      Pending -> do
        let target = dist ++ "-build"
        -- FIXME: handle case of no build
        latest <- kojiLatestPkg target pkg
        unless (eqNVR nvr latest) $
          putStrLn $ latest +-+ "->" +-+ nvr
      Changed -> do
        let target = dist ++ "-build"
        -- FIXME: handle case of no build
        latest <- kojiLatestPkg target pkg
        unless (eqNVR nvr latest) $
          putStrLn pkg

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir m =
    bracket
        (do cwd <- getCurrentDirectory
            exists <- doesDirectoryExist dir
            if exists
              then setCurrentDirectory dir
              else error $ "Cannot set non-existent directory" +-+ dir
            return cwd)
        setCurrentDirectory
        (const m)

maybePkgVer :: String -> Maybe String -> String
maybePkgVer pkg mver = pkg ++ maybe "" ("-" ++) mver

notInstalled :: (String, Maybe String) -> IO Bool
notInstalled (pkg, mver) =
  not <$> cmdBool "rpm" ["--quiet", "-q", maybePkgVer pkg mver]

fhbuildMissing :: String -> String -> IO ()
fhbuildMissing dist dep = do
  base <- derefSrcPkg dep
  build Install dist Nothing (Just dep) base

derefPkg :: (String, Maybe String) -> IO (String, Maybe String)
derefPkg (pkg, mver) = do
  res <- singleLine <$> cmd "repoquery" ["--qf", "%{name}", "--whatprovides", pkg]
  return (res, mver)

derefSrcPkg:: String -> IO String
derefSrcPkg pkg = singleLine <$> cmd "repoquery" ["--qf", "%{base_package_name}", "--whatprovides", pkg]

gitBranch :: IO String
gitBranch =
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["branch"]

eqNVR :: String -> String -> Bool
eqNVR p1 p2 =
  dropExtension p1 == dropExtension p2

processDeps :: [String] -> (String, Maybe String)
processDeps [p, "=", v] = (p, Just v)
processDeps (p:_) = (p, Nothing)
processDeps [] = error "processDeps: empty string!"

