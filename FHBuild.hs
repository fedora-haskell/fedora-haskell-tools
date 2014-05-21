{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      :  Main
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

import Control.Monad (void)
import Data.Monoid ((<>))
import Prelude hiding (FilePath)
import Shelly
import qualified Data.Text as T

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeBaseName, takeDirectory)
import System.IO (hPutStrLn, stderr)

default (T.Text)

main :: IO ()
main = do
  (com:dist:pkgs) <- getArgs >>= parseArgs
  shelly $
    mapM_ (command' com (T.pack dist) . T.pack) pkgs
  where
    command' "local" = buildLocal
--    command' "koji" = buildKoji
    command' _ = undefined

commands :: [String]
commands = ["local" {-, "koji"-}]

parseArgs :: [String] -> IO [String]
parseArgs [] = help >> return []
parseArgs [com] | com `elem` commands = do
  dir <- getCurrentDirectory
  (pkg, branch) <- determinePkgBranch dir
  return [com, branch, pkg]
parseArgs args | (head args `elem` commands && length args >= 3) =
  return args
parseArgs _ = help >> return []

determinePkgBranch :: String -> IO (String, String)
determinePkgBranch dir = do
  let base = takeBaseName dir
  if base `elem` ["master", "f20", "f19"]
    then return (takeBaseName $ takeDirectory dir, base)
    else do
    branch <- gitBranch
    return (base, branch)

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage: " ++ progName ++ " CMD dist pkg ...\n"
    ++ "\n"
    ++ "Commands:\n"
    ++ "  local\t\t- build locally\n"
    ++ "  koji\t\t- build in Koji\n"
  exitWith (ExitFailure 1)

dist2branch :: T.Text -> T.Text
dist2branch "f21" = "master"
dist2branch d = d

buildLocal :: T.Text -> T.Text -> Sh ()
buildLocal dist pkg = sub $ do
  d <- test_d $ fromText pkg
  let branch = fromText $ dist2branch dist
  unless d $
    cmd "fedpkg" "clone" "-b" branch pkg
  cd $ fromText pkg
  whenM (test_d branch) $ cd branch
  echo $ "== " <> pkg <> ":" <> dist2branch dist <> " =="
  when d $
    -- todo check(out) correct branch
    cmd "git" "pull"
  nvr <- cmd "fedpkg" "verrel"
  installed <- cmd "rpm" "-q" "--qf" "%{name}-%{version}-%{release}" pkg
  when (nvr == installed) $ do
    echo $ T.stripEnd nvr <> " already installed!"
    exit 0
  echo $ installed <> " -> " <> nvr
  run_ "git" ["log", "-2"]
  void $ cmd "sudo" "yum-builddep" "-y" $ pkg <.> "spec"
  run_ "fedpkg" ["local"]
  void $ cmd "sudo" "yum" "remove" pkg
  void $ cmd "sudo" "yum" "localinstall" $ "x86_64" </> "*"
  exit 0

-- FIXME
gitBranch :: IO (String)
gitBranch = return "f20"
