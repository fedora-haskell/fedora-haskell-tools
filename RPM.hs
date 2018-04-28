-- |
-- Module      :  RPM
-- Copyright   :  (C) 2015-2017  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: RPM utils

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module RPM (packageManager,
            repoquery,
            repoquerySrc,
            rpmInstall,
            rpmspec) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)
import Data.Maybe (isJust, isNothing)
import System.Directory (findExecutable)
import System.FilePath ((</>))
-- die is available in ghc-7.10 base-4.8
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Dists (Dist, distTag)
import Utils (cmd, sudo)

-- @since base 4.8.0.0
die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

requireProgram :: String -> IO ()
requireProgram c = do
  mavail <- findExecutable c
  when (isNothing mavail) $ die (c ++ ": command not found")

optionalProgram :: String -> IO Bool
optionalProgram c = isJust <$> findExecutable c

packageManager :: IO String
packageManager = do
  havednf <- optionalProgram "dnf"
  if havednf
    then return "dnf"
    else requireProgram "yum" >> return "yum"

rpmInstall :: [String] -> IO ()
rpmInstall rpms = do
  pkginstaller <- packageManager
  let (inst, arg) = if pkginstaller == "dnf" then ("dnf", "install") else ("yum", "localinstall")
  sudo inst $ ["-y", "--nogpgcheck", arg] ++ rpms

repoquery :: String -> [String] -> IO String
repoquery relver args = do
  havednf <- optionalProgram "dnf"
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "--quiet"]) else ("repoquery", [])
  cmd prog (subcmd ++ ["--releasever=" ++ relver] ++ args)

repoquerySrc :: Dist -> String -> String -> IO (Maybe String)
repoquerySrc dist relver key = do
  havednf <- optionalProgram "dnf"
  let srcflag = if havednf then ["--qf=%{source_name}"] else ["--qf", "%{base_package_name}"]
  res <- words <$> repoquery relver (srcflag ++ ["--repofrompath", "koji,http://kojipkgs.fedoraproject.org/repos" </> distTag dist </> "latest/x86_64/", "--whatprovides", key])
  return $ case res of
    [p] -> Just p
    ps | key `elem` ps -> Just key
    _ -> Nothing

rpmspec :: [String] -> Maybe String -> FilePath -> IO String
rpmspec args mqf spec = do
  let qf = maybe [] (\ q -> ["--queryformat", q]) mqf
  cmd "rpmspec" (["-q"] ++ args ++ qf ++ [spec])
