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

import HSH

import Control.Monad (when, unless)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), takeBaseName, takeDirectory)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  (com:dist:pkgs) <- getArgs >>= parseArgs
  mapM_ (command' com dist) pkgs
  where
    command' "local" = buildLocal
--    command' "koji" = buildKoji
    command' _ = undefined

commands :: [String]
commands = ["local" {-, "koji"-}]

parseArgs :: [String] -> IO [String]
parseArgs [] = help >> return []
parseArgs [com] | com `elem` commands = do
  dir <- pwd
  (pkg, branch) <- determinePkgBranch dir
  return [com, branch, pkg]
parseArgs args | head args `elem` commands && length args >= 3 =
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

dist2branch :: String -> String
dist2branch "f21" = "master"
dist2branch d = d

cmd :: String -> [String] -> IO String
cmd c as = run (c, as)

cmd_ :: String -> [String] -> IO ()
cmd_ c as = runIO (c, as)

cmdSL :: String -> [String] -> IO String
cmdSL c as = runSL (c, as)

sudo :: String -> [String] -> IO ()
sudo c as = do
  putStrLn $ c ++ unwords as
  runIO ("sudo", c:as)

buildLocal :: String -> String -> IO ()
buildLocal dist pkg = do
  d <- doesDirectoryExist pkg
  let branch = dist2branch dist
  unless d $
    cmd_ "fedpkg" ["clone", "-b", branch, pkg]
  cd pkg
  b <- doesDirectoryExist branch
  when b $ cd branch
  putStrLn $ "== " ++ pkg ++ ":" ++ dist2branch dist ++ " =="
  when d $
    -- todo check(out) correct branch
    cmd_ "git" ["pull"]
  nvr <- cmdSL "fedpkg" ["verrel"]
  installed <- cmd "rpm" ["-q", "--qf", "%{name}-%{version}-%{release}", pkg]
  if nvr == installed
    then putStrLn $ nvr ++ " already installed!"
    else do
    putStrLn $ installed ++ " -> " ++ nvr
    cmd_ "git" ["log", "-2"]
    sudo "yum-builddep" ["-y", pkg ++ ".spec"]
    cmd_ "fedpkg" ["local"]
    sudo "yum" ["remove", pkg]
    sudo "yum" ["localinstall", "x86_64" </> "*"]

-- FIXME
gitBranch :: IO String
gitBranch = return "f20"
