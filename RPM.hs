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

import Control.Monad (when)
import Data.Maybe (isJust, isNothing)
import System.Directory (findExecutable)
-- die is available in ghc-7.10 base-4.8
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Utils (cmd, sudo)

-- @since base 4.8.0.0
die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

requireProgram :: String -> IO ()
requireProgram c = do
  mavail <- findExecutable c
  when (isNothing mavail) $ die (c ++ ": command not found")

optionalProgram :: String -> IO Bool
optionalProgram c = do
  mavail <- findExecutable c
  return $ isJust mavail

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
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "--quiet", "--releasever=" ++ relver]) else ("repoquery", ["--releasever=" ++ relver])
  cmd prog (subcmd ++ args)

-- repoqWrap :: String -> [String] -> IO String
-- repoqWrap c args = do
--   (out, err) <- cmdStdErr c args
--   -- workaround noisy dnf2 repoquery --quiet
--   -- ignore "Last metadata expiration check" warnings
--   unless (null err || head (words err) == "Last") $
--     warn err
--   return $ singleLine out

repoquerySrc :: String -> IO (Maybe String)
repoquerySrc key = do
  havednf <- optionalProgram "dnf"
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "--quiet", "--qf=%{source_name}"]) else ("repoquery", ["--qf", "%{base_package_name}", "--whatprovides"])
  res <- cmd prog (subcmd ++ [key])
  if null res then return Nothing
    else return $ Just res

rpmspec :: [String] -> Maybe String -> FilePath -> IO String
rpmspec args mqf spec = do
  let qf = maybe [] (\ q -> ["--queryformat", q]) mqf
  cmd "rpmspec" (["-q"] ++ args ++ qf ++ [spec])
