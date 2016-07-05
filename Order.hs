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

import Control.Applicative ((<$>))

import Data.List (isPrefixOf, (\\))

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Dists
import Utils

data Command = Install | Mock | Koji | Pending | Changed | Built deriving (Eq)

main :: IO ()
main = do
  (_dist, pkgs) <- getArgs >>= parseArgs
  pkgdeps <- mapM pkgDeps pkgs
  let sorted = sort pkgdeps
  putStrLn $ unwords $ sorted

parseArgs :: [String] -> IO (String, [String])
parseArgs (dist:pkgs) | dist `elem` dists && not (null pkgs) =
                          return (dist, map (removeSuffix "/") pkgs)
parseArgs _ = help >> return ("", [])

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "[dist] [pkg] ..."
  exitWith (ExitFailure 1)

type PackageDeps = (String, [String])

pkgDeps :: String -> IO PackageDeps
pkgDeps pkg = do
  deps <- lines <$> cmd "rpmspec" ["-q", "--buildrequires", pkg </> "master" </> pkg ++ ".spec"]
  return (pkg, deps)

sort :: [PackageDeps] -> [String]
sort [] = []
sort ((p,deps):rest) =
  sort lesser ++ [p] ++ sort greater
  where
    lesser = filter (isDep deps) rest
    greater = rest \\ lesser

isDep :: [String] -> PackageDeps -> Bool
isDep [] _ = False
isDep deps (p, _) = (maybeSuffix p `elem` deps)
  where
    maybeSuffix pkg | "ghc-" `isPrefixOf` pkg = pkg ++ "-devel"
                    | otherwise = pkg

