-- |
-- Module      :  Order
-- Copyright   :  (C) 2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: Utility to order packages with respect to BuildRequires

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif

import Data.List (isPrefixOf, (\\))
import Data.Maybe (catMaybes)

import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Dists
import Utils

main :: IO ()
main = do
  (dist, pkgs) <- getArgs >>= parseArgs
  sorted <- sortPkgs dist pkgs
  putStrLn $ unwords $ map fst sorted

parseArgs :: [String] -> IO (String, [String])
parseArgs (dist:pkgs) | dist `elem` dists && not (null pkgs) =
                          return (dist, map (removeSuffix "/") pkgs)
parseArgs _ = help >> return ("", [])

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "[dist] [pkg] ..."
  exitWith (ExitFailure 1)

sortPkgs :: String -> [String] -> IO [PackageDeps]
sortPkgs dist pkgs = do
  pkgdeps <- catMaybes <$> mapM (pkgDeps $ distBranch dist) pkgs
  return $ sort pkgdeps

type PackageDeps = (String, [String])

pkgDeps :: String -> String -> IO (Maybe PackageDeps)
pkgDeps branch pkg = do
  dirExists <- doesDirectoryExist $ pkg </> branch
  let branchdir = if dirExists then branch else ""
      file = pkg </> branchdir </> pkg ++ ".spec"
  fileExists <- doesFileExist file
  if fileExists
    then do
    deps <- lines <$> cmd "rpmspec" ["-q", "--buildrequires", file]
    return $ Just (pkg, deps)
    else return Nothing

sort :: [PackageDeps] -> [PackageDeps]
sort [] = []
sort (pd@(_p,deps):rest) =
  sort lesser ++ [pd] ++ sort greater
  where
    lesser = filter (isDep deps) rest
    greater = rest \\ lesser

isDep :: [String] -> PackageDeps -> Bool
isDep [] _ = False
isDep deps (p, _) = maybeSuffix p `elem` deps || p `elem` deps
  where
    maybeSuffix pkg | "ghc-" `isPrefixOf` pkg = pkg ++ "-devel"
                    | otherwise = "ghc-" ++ pkg ++ "-devel"

