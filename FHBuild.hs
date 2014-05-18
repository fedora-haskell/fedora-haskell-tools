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

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.IO (Handle, hPutStrLn, stderr)

default (T.Text)

main :: IO ()
main = do
  args <- getArgs
  when (length args < 3) $ help stderr >> exitWith (ExitFailure 1)
  let (com:dist:pkgs) = args
  shelly $
    mapM_ (command' com (T.pack dist) . T.pack) pkgs
  where
    command' "local" = buildLocal
--    command' "koji" = buildKoji
    command' _ = undefined
    
help :: Handle -> IO ()
help h = do
  progName <- getProgName
  hPutStrLn h $ "Usage: " ++ progName ++ " CMD dist pkg ..."
    ++ "\n"
    ++ "Commands:\n"
    ++ "  local\t\t- build locally\n"
    ++ "  koji\t\t- build in Koji\n"

branch :: T.Text -> T.Text
branch "f21" = "master"
branch d = d

buildLocal :: T.Text -> T.Text -> Sh ()
buildLocal dist pkg = sub $ do
  d <- test_d $ fromText pkg
  let brnch = fromText $ branch dist
  unless d $
    cmd "fedpkg" "clone" brnch pkg
  cd $ fromText pkg
  whenM (test_d brnch) $ cd brnch
  echo $ "== " <> pkg <> ":" <> branch dist <> " =="
  when d $
    -- todo check(out) correct branch
    cmd "git" "pull"
  nvr <- cmd "fedpkg" "verrel"
  installed <- cmd "rpm" "-q" "--qf" "%{name}-%{version}-%{release}" pkg
  when (nvr == installed) $ do
    echo $ nvr <> " already installed!"
    exit 0
  echo $ installed <> " -> " <> nvr
  run_ "git" ["log", "-2"]
  void $ cmd "sudo" "yum-builddep" "-y" $ pkg <.> "spec"
  run_ "fedpkg" ["local"]
  void $ cmd "sudo" "yum" "remove" pkg
  void $ cmd "sudo" "yum" "localinstall" $ "x86_64" </> "*"
  exit 0
