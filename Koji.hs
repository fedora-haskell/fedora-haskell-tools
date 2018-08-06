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

module Koji (kojiBuilding,
             kojiCheckFHBuilt,
             kojicmd,
             kojiLatestPkg,
             kojiListPkgs,
             kojiWaitPkg,
             notInKoji,
             rpkg,
             rpkgBuild
            ) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Data.List (isInfixOf, isPrefixOf)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.FilePath ((</>))

import Dists (Dist, distTarget)
import RPM (pkgDir)
import Utils (cmd, cmd_, cmdBool, cmdFragile, cmdFragile_, grep, logMsg)

kojicmd :: Dist -> String
kojicmd dist = if "rhel" `isPrefixOf` dist then "brew" else "koji"

kojisafe :: Dist -> String -> [String] -> IO String
kojisafe dist c as =
  cmdFragile (kojicmd dist) (c:as)

kojiLatestPkg :: Dist -> String -> IO (Maybe String)
kojiLatestPkg dist pkg = do
  res <- words <$> kojisafe dist "latest-pkg" ["--quiet", dist, pkg]
  return $ if null res then Nothing else Just $ head res

kojiWaitPkg :: FilePath -> Dist -> String -> IO ()
kojiWaitPkg topdir dist nvr = do
  let fhbuilt = topdir </> ".fhbuilt"
  already <- kojiCheckFHBuilt topdir nvr
  unless already $ do
    cmdFragile_ (kojicmd dist) ["wait-repo", dist, "--build=" ++ nvr]
    appendFile fhbuilt $ nvr ++ "\n"

kojiCheckFHBuilt :: FilePath -> String -> IO Bool
kojiCheckFHBuilt topdir nvr = do
  let fhbuilt = topdir </> ".fhbuilt"
  grep nvr fhbuilt

kojiBuilding :: String -> String -> Dist -> IO Bool
kojiBuilding pkg build dist = do
  tasks <- lines <$> kojisafe dist "list-tasks" ["--mine", "--quiet"]
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
  words <$> cmd (kojicmd dist) ["list-pkgs", "--tag=" ++ dist]

rpkg :: Maybe String -> String
rpkg Nothing = "fedpkg"
rpkg (Just dist) = if "rhel" `isPrefixOf` dist then "rhpkg" else "fedpkg"

rpkgBuild :: FilePath -> Dist -> String -> Maybe String -> IO ()
rpkgBuild topdir dist nvr waittag = do
  giturl <- cmd (rpkg (Just dist)) ["giturl"]
  out <- cmd (kojicmd dist) ["build", "--nowait", "--fail-fast", distTarget dist, giturl]
  putStrLn out
  let task = last . words . head $ lines out
  start <- getCurrentTime
  success <- cmdBool "koji" ["watch-task", task]
  if success
    then do
    logMsg $ nvr ++ " built"
    maybe (return ()) (\ t -> kojiWaitPkg topdir t nvr) waittag
    else do
    now <- getCurrentTime
    -- koji srpms typically take 2 minutes
    let countdown = 120 - round (diffUTCTime now start) :: Int
    when (countdown >= 0) $
      cmd_ "sleep" [show countdown]
