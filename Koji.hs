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
import Data.List (isInfixOf)
import System.FilePath ((</>))

import Utils (cmd, cmd_)

kojiLatestPkg :: String -> String -> IO String
kojiLatestPkg dist pkg = do
  res <- words <$> cmd "koji" ["latest-pkg", "--quiet", dist, pkg]
  return $ if null res then "" else head res

kojiWaitPkg :: String -> String -> IO ()
kojiWaitPkg dist pkg = --do
  cmd_ "koji" ["wait-repo", dist, "--build=" ++ pkg]
  --putStrLn ""

kojiBuilding :: String -> String -> IO Bool
kojiBuilding pkg build = do
  tasks <- lines <$> cmd "koji" ["list-tasks", "--mine", "--quiet"]
  return $ any (build `isInfixOf`) tasks || any (("/" ++ pkg ++ ":") `isInfixOf`) tasks

-- parseKojiTask :: [String] -> Maybe String
-- parseKojiTask [] = Nothing
-- parseKojiTask (l:ls) | "Created task:" `isPrefixOf` l = Just $ removePrefix "Created task: " l
--                       | otherwise = parseKojiTask ls

notInKoji :: String -> FilePath -> String -> String -> IO Bool
notInKoji branch topdir tag pkg = do
  latest <- kojiLatestPkg tag pkg
  local <- cmd "fedpkg" ["--path", topdir </> pkg </> branch, "verrel"]
  if latest == local
    then kojiWaitPkg tag latest >> return False
    else return True
