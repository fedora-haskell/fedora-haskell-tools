-- |
-- Module      :  Koji
-- Copyright   :  (C) 2016-2017  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  *nix
--
-- Explanation: Koji commands

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Koji where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless)
import Data.List (isInfixOf, isPrefixOf)
import System.FilePath ((</>))

import Dists (Dist)
import RPM (pkgDir)
import Utils (cmd, cmd_, cmdBool, grep)

koji :: Dist -> String
koji dist = if "rhel" `isPrefixOf` dist then "brew" else "koji"

kojiLatestPkg :: Dist -> String -> IO (Maybe String)
kojiLatestPkg dist pkg = do
  res <- words <$> cmd (koji dist) ["latest-pkg", "--quiet", dist, pkg]
  return $ if null res then Nothing else Just $ head res

kojiWaitPkg :: FilePath -> Dist -> String -> IO ()
kojiWaitPkg topdir dist nvr = do
  let fhbuilt = topdir </> ".fhbuilt"
  already <- kojiCheckFHBuilt topdir nvr
  unless already $ do
    cmd_ (koji dist) ["wait-repo", dist, "--build=" ++ nvr]
    appendFile fhbuilt $ nvr ++ "\n"

kojiCheckFHBuilt :: FilePath -> String -> IO Bool
kojiCheckFHBuilt topdir nvr = do
  let fhbuilt = topdir </> ".fhbuilt"
  grep nvr fhbuilt

kojiBuilding :: String -> String -> Dist -> IO Bool
kojiBuilding pkg build dist = do
  tasks <- lines <$> cmd (koji dist) ["list-tasks", "--mine", "--quiet"]
  return $ any (build `isInfixOf`) tasks || any (("/" ++ pkg ++ ":") `isInfixOf`) tasks

-- parseKojiTask :: [String] -> Maybe String
-- parseKojiTask [] = Nothing
-- parseKojiTask (l:ls) | "Created task:" `isPrefixOf` l = Just $ removePrefix "Created task: " l
--                       | otherwise = parseKojiTask ls

notInKoji :: String -> FilePath -> String -> String -> IO Bool
notInKoji branch topdir tag pkg = do
  latest <- kojiLatestPkg tag pkg
  pkgpath <- pkgDir pkg branch topdir
  local <- cmd (rpkg (Just tag)) ["--path", pkgpath, "verrel"]
  if latest == Just local
    then kojiWaitPkg topdir tag local >> return False
    else return True

kojiListPkgs :: Dist -> IO [String]
kojiListPkgs dist =
  words <$> cmd (koji dist) ["list-pkgs", "--tag=" ++ dist]

rpkg :: Maybe String -> String
rpkg Nothing = "fedpkg"
rpkg (Just dist) = if "rhel" `isPrefixOf` dist then "rhpkg" else "fedpkg"
