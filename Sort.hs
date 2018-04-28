-- |
-- Module      :  Sort
-- Copyright   :  (C) 2017  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  *nix
--
-- Explanation: package build order sorting

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Sort (sortPkgs, PackageDeps) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif

import Data.List (isPrefixOf, (\\))
import Data.Maybe (catMaybes)

import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Dists
import Utils

sortPkgs :: String -> [String] -> IO [PackageDeps]
sortPkgs dist pkgs =
  sort . catMaybes <$> mapM (pkgDeps $ distBranch dist) pkgs

type PackageDeps = (String, [String])

pkgDeps :: String -> String -> IO (Maybe PackageDeps)
pkgDeps branch pkg = do
  dirExists <- doesDirectoryExist $ pkg </> branch
  let branchdir = if dirExists then branch else ""
      file = pkg </> branchdir </> pkg ++ ".spec"
  fileExists <- doesFileExist file
  if fileExists
    then do
    deps <- lines <$> rpmspec ["--buildrequires"] Nothing file
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

