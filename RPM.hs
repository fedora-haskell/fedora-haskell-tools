-- |
-- Module      :  RPM
-- Copyright   :  (C) 2015  Jens Petersen
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
            rpmInstall) where

import Control.Monad (unless, when)
import Data.Maybe       (isJust, isNothing)
import System.Directory (findExecutable)
-- die is available in ghc-7.10 base-4.8
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Utils (cmdStdErr, sudo)

-- @since base 4.8.0.0
die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

warn :: String -> IO ()
warn err = hPutStrLn stderr err

requireProgram :: String -> IO ()
requireProgram c = do
  mavail <- findExecutable c
  when (isNothing mavail) $ die (c ++ ": command not found")

optionalProgram :: String -> IO Bool
optionalProgram c = do
  mavail <- findExecutable c
  pure $ isJust mavail

packageManager :: IO String
packageManager = do
  havednf <- optionalProgram "dnf"
  if havednf
    then pure "dnf"
    else requireProgram "yum" >> pure "yum"

repoquery :: [String] -> String -> IO String
repoquery args key = do
  havednf <- optionalProgram "dnf"
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "--quiet"]) else ("repoquery", [])
  repoqWrap prog (subcmd ++ args ++ [key])

repoqWrap :: String -> [String] -> IO String
repoqWrap cmd args = do
  (out, err) <- cmdStdErr cmd args
  -- workaround noisy dnf2 repoquery --quiet
  -- ignore "Last metadata expiration check" warnings
  unless (null err || head (words err) == "Last") $
    warn err
  pure out

repoquerySrc :: String -> IO String
repoquerySrc key = do
  havednf <- optionalProgram "dnf"
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "--quiet", "--srpm", "--qf", "%{name}"]) else ("repoquery", ["--qf", "%{base_package_name}", "--whatprovides"])
  repoqWrap prog (subcmd ++ [key])

rpmInstall :: [String] -> IO ()
rpmInstall rpms = do
  pkginstaller <- packageManager
  let (inst, arg) = if pkginstaller == "dnf" then ("dnf", "install") else ("yum", "localinstall")
  sudo inst $ ["-y", "--nogpgcheck", arg] ++ rpms
