-- |
-- Module      :  FHKoji
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
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, stripPrefix)

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), dropExtension)
import System.IO (hPutStrLn, stderr)

import Utils

main :: IO ()
main = do
  (dist, pkgs) <- getArgs >>= parseArgs
  -- sort pkgs
  mapM (prepPkg dist) pkgs >>= buildDriver dist 
--  buildKoji plan

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "[dist] [pkg] ..."
  exitWith (ExitFailure 1)

dists :: [String]
dists = [rawhide, "f20", "f19", "epel7"]

rawhide :: String
rawhide = "f21"

parseArgs :: [String] -> IO (String, [String])
parseArgs (dist:pkgs) |  dist `elem` dists && not (null pkgs) =
  return (dist,pkgs)
parseArgs _ = help >> return ("", [])

dist2branch :: String -> String
dist2branch d | d == rawhide = "master"
              | otherwise = d

prepPkg :: String -> String -> IO (String, String, FilePath)
prepPkg dist pkg = do
  let branch = dist2branch dist
      dir = pkg
  dirExists <- doesDirectoryExist dir
  putStrLn $ "\n==" +-+ pkg ++ ":" ++ dist2branch dist +-+ "=="
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
      return (pkg, nvr, wd)

buildKoji :: String -> String -> String -> FilePath -> IO ()
buildKoji dist pkg nvr wd = do
    cmdlog "fedpkg" ["--path", wd, "build"]
    when (dist /= rawhide) $ do
      user <- shell "grep Subject: ~/.fedora.cert | sed -e 's@.*CN=\\(.*\\)/emailAddress=.*@\\1@'"
      -- FIXME: improve Notes with recursive info
      cmdlog "bodhi" ["-o", nvr, "-u", user, "-N", pkg +-+ "stack"]
      cmdlog "koji" ["wait-repo", dist ++ "-build", "--build=" ++ nvr]

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

derefPkg :: (String, Maybe String) -> IO (String, Maybe String)
derefPkg (pkg, mver) = do
  res <- singleLine <$> cmd "repoquery" ["--qf", "%{name}", "--whatprovides", pkg]
  return (res, mver)

derefSrcPkg:: String -> IO String
derefSrcPkg pkg = singleLine <$> cmd "repoquery" ["--qf", "%{base_package_name}", "--whatprovides", pkg]

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
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["branch"]

eqNVR :: String -> String -> Bool
eqNVR p1 p2 =
  dropExtension p1 == dropExtension p2

processDeps :: [String] -> (String, Maybe String)
processDeps [p, "=", v] = (p, Just v)
processDeps (p:_) = (p, Nothing)
processDeps [] = error "convEquals: empty string!"

buildDriver :: String -> [(String, String, FilePath)] -> IO ()
buildDriver dist plan = return ()
