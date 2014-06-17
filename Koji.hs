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
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (find, isPrefixOf, nub, stripPrefix, union)

import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Dists
import Utils

-- (name, (nvr, dir, cabal)) -- add deps too
type BuildStep = (String, (String, FilePath, FilePath))

main :: IO ()
main = do
  (dist, pkgs) <- getArgs >>= parseArgs
  plan <- mapM (prepPkg dist) pkgs
  sorted <- cabalSort plan
  putStrLn $ "\nBuilding:" +-+ (unwords $ map fst sorted)
  -- FIXME check plan with "cblrepo -n add"
  -- currently need packagedb-cli.git for pkgdb2
  hsPkgs <- words <$> cmd "pkgdb-cli" ["list", "--branch", distBranch dist, "--user", "haskell-sig", "--nameonly"]
  buildDriver dist hsPkgs plan [] sorted

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
  putStrLn $ "\n== Prepare" +-+ pkg ++ ":" ++ branch +-+ "=="
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
      dirBranch <- gitBranch wd
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
    cmd_ "git" ["--no-pager", "-C", wd, "log", "-1"]
    -- FIXME: handle case of no build
    latest <- (head . words) <$> cmd "koji" ["latest-pkg", "--quiet", dist ++ "-build", pkg]
    if nvr == latest
      then error $ nvr +-+ "already built!"
      else do
      putStrLn $ "\n" ++ latest +-+ "->" +-+ nvr
      let cabal = cabalFile wd pkg nvr
      return (pkg, (nvr, wd, cabal))

cabalFile :: FilePath -> String -> String -> FilePath
cabalFile wd pkg nvr = wd </> hkg ++ "-" ++ ver </> hkg ++ ".cabal"
  where
    ver = nvrVersion nvr
    hkg = removeghc pkg

removeghc :: String -> String
removeghc pkg = fromMaybe pkg $ stripPrefix "ghc-" pkg

nvrVersion :: String -> String
nvrVersion nvr =
  if '-' `notElem` nvr
  then error "nvrVersion: malformed NVR string" +-+ nvr
  else reverse rev
  where
    (_, '-':rest) = break (== '-') $ reverse nvr
    (rev, _) = break (== '-') rest

cabalSort :: [BuildStep] -> IO [BuildStep]
cabalSort bs = do
  let cabals = map (\(_,(_,_,c)) -> c) bs
  sorted <- lines <$> cmd "cabal-sort" cabals
  return $ catMaybes $ map (\ p -> find (\ (_, (_, _, pth)) -> p == pth) bs) sorted

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

gitBranch :: FilePath -> IO String
gitBranch wd =
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["-C", wd, "branch"]

buildDriver :: String -> [String] -> [BuildStep] -> [String] -> [BuildStep] -> IO ()
buildDriver _ _ _ _ [] = return ()
buildDriver dist hspkgs plan buildroot ((pkg, (nvr, wd, _)):rest) = do
  putStrLn $ "\n== Building" +-+ nvr +-+ "=="
  let spec = wd </> pkg ++ ".spec"
  -- extract pkg from "pkg = ver"
  deps <- (map (head . words) . lines) <$> cmd "rpmspec" ["-q", "--buildrequires", spec] >>= mapM derefSrcPkg
  let hdeps = filter (`elem` hspkgs) $ nub deps
  depNVRs <- mapM (latestInBuildRoot dist plan buildroot) hdeps
  buildKoji dist pkg nvr wd
  buildDriver dist hspkgs plan (union buildroot depNVRs) rest

-- FIXME cache results
latestInBuildRoot :: String -> [BuildStep] -> [String] -> String -> IO String
latestInBuildRoot dist plan buildroot pkg = do
  let inPlan = lookup pkg plan
  nvr <- case inPlan of
    Just (nvr', _, _) -> return nvr'
    Nothing -> kojiLatestPkg (dist ++ "-build") pkg
  -- FIXME check pkg git nvr?
  unless (nvr `elem` buildroot) $
    cmdlog "koji" ["wait-repo", dist ++ "-build", "--build", nvr]
  return nvr
