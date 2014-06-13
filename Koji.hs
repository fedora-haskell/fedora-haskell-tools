-- |
-- Module      :  Koji
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Building RPM packages in Fedora Koji buildsystem.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

import Control.Applicative ((<$>))
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, stripPrefix, (\\))

import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Dists
import Utils

-- (name, (nvr, dir)) -- add deps too
type BuildStep = (String, (String, FilePath))

main :: IO ()
main = do
  (dist, pkgs) <- getArgs >>= parseArgs
  -- FIXME cabal-sort pkgs
  plan <- mapM (prepPkg dist) pkgs
  -- FIXME check plan with "cblrepo -n add"
  -- currently need packagedb-cli.git
  hsPkgs <- lines <$> cmd "pkgdb-cli" ["list", "--branch", distBranch dist, "--user", "haskell-sig", "--nameonly"]
  buildDriver dist (hsPkgs \\ ["ghc"]) plan plan

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "[dist] [pkg] ..."
  exitWith (ExitFailure 1)

parseArgs :: [String] -> IO (String, [String])
parseArgs (dist:pkgs) |  dist `elem` dists && not (null pkgs) =
  return (dist, pkgs)
parseArgs pkgs | not (null pkgs) = return (rawhide, pkgs)
parseArgs _ = help >> return ("", [])

prepPkg :: String -> String -> IO BuildStep
prepPkg dist pkg = do
  let branch = distBranch dist
      dir = pkg
  dirExists <- doesDirectoryExist dir
  putStrLn $ "\nPrep =" +-+ pkg ++ ":" ++ branch +-+ "="
  unless dirExists $ do
    let anon = ["-a"]
    cmdlog "fedpkg" $ ["clone", "-b", branch, pkg] ++ anon
  b <- doesDirectoryExist $ dir </> branch
  let wd = dir </> if b then branch else ""
  retired <- doesFileExist $ wd </> "dead.package"
  if retired
    then error $ pkg +-+ "is retired!"
    else do
    cmdAssert "not a Fedora pkg git dir!" "grep" ["-q", "pkgs.fedoraproject.org", wd </> ".git/config"]
    when dirExists $ do
      dirBranch <- gitBranch
      when (dirBranch /= branch) $
        cmd_ "fedpkg" ["--path", wd, "switch-branch", branch]
      diffOrig <- cmd "git" ["-C", wd, "diff", "origin" </> branch]
      unless (null diffOrig) $
        error $ pkg +-+ "has unpushed changes!"
      cmd_ "git" ["-C", wd, "pull", "-q"]
      spec <- cmd "fedpkg" ["--path", wd, "gimmespec"]
      unless (spec == pkg ++ ".spec") $
        error $ pkg ++ ": inconsistent spec file" +-+ spec
    nvr <- cmd "fedpkg" ["--path", wd, "verrel"]
--    let verrel = removePrefix (pkg ++ "-") nvr
    cmd_ "git" ["--no-pager", "-C", wd, "log", "-1"]
    -- FIXME: handle case of no build
    latest <- (head . words) <$> cmd "koji" ["latest-pkg", "--quiet", dist ++ "-build", pkg]
    if nvr == latest
      then error $ nvr +-+ "already built!"
      else do
      putStrLn $ latest +-+ "->" +-+ nvr
      return (pkg, (nvr, wd))

buildKoji :: String -> String -> String -> FilePath -> IO ()
buildKoji dist pkg nvr wd = do
    cmdlog "fedpkg" ["--path", wd, "build"]
    when (distOverride dist) $ do
      user <- shell "grep Subject: ~/.fedora.cert | sed -e 's@.*CN=\\(.*\\)/emailAddress=.*@\\1@'"
      -- FIXME: improve Notes with recursive info
      -- check if any rdeps need this build
      cmdlog "bodhi" ["-o", nvr, "-u", user, "-N", pkg +-+ "stack"]

-- FIXME: need --repoid=$REPOID --releasever=$RELEASEVER
derefSrcPkg:: String -> IO String
derefSrcPkg pkg = singleLine <$> cmd "repoquery" ["--qf", "%{base_package_name}", "--whatprovides", pkg]

removePrefix :: String -> String -> String
removePrefix prefix orig =
  fromMaybe (error prefix +-+ "is not prefix of" +-+ orig) $ stripPrefix prefix orig

gitBranch :: IO String
gitBranch =
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["branch"]

buildDriver :: String -> [String] -> [BuildStep] -> [BuildStep] -> IO ()
buildDriver _ _ _ [] = return ()
buildDriver dist hspkgs plan ((pkg, (nvr, wd)):rest) = do
  let spec = wd </> pkg ++ ".spec"
  deps <- (map (head . words) . lines) <$> cmd "rpmspec" ["-q", "--buildrequires", spec] >>= mapM derefSrcPkg
  let hdeps = filter (`elem` hspkgs) deps
  unless (null hdeps) $
    mapM_ (latestInBuildRoot dist plan) hdeps
  buildKoji dist pkg nvr wd
  buildDriver dist hspkgs plan rest

latestInBuildRoot :: String -> [BuildStep] -> String -> IO ()
latestInBuildRoot dist plan pkg = do
  let inplan = lookup pkg plan
  latest <- kojiLatestPkg (dist ++ "-build") pkg
  case inplan of
    Just (nvr, _) ->
      if nvr == latest
      then cmd_ "koji" ["wait-repo", dist ++ "-build", "--build", latest]
      else error $ dist ++ "-build" +-+ "has" +-+ latest +-+ "not" +-+ nvr
    Nothing ->
      -- FIXME check pkg git nvr
      cmd_ "koji" ["wait-repo", dist ++ "-build", "--build", latest]
