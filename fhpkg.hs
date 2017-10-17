-- |
-- Module      :  Packages
-- Copyright   :  (C) 2017  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: Cloning and pulling package git repos

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- Todo:
-- cache dist package lists
-- hackage data
-- detect subpackages

module Main where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Data.Maybe
import Data.List (isPrefixOf, nub, sort)

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
--import System.Posix.Env (getEnv)

import Dists (Dist, dists, distBranch)
import Koji (kojiListPkgs)
import Utils ((+-+), cmd, cmd_, cmdBool, removePrefix)

data Command = Clone | Pull | List | Count | Diff | Hackage deriving (Eq)

main :: IO ()
main = do
  margs <- getArgs >>= parseArgs
  case margs of
    Nothing -> return ()
    Just (com, mdist, pkgs) -> do
      cwd <- getCurrentDirectory
      ps <- if null pkgs then kojiListHaskell True mdist else return pkgs
      case mode com of
        List -> mapM_ putStrLn ps
        Count -> print $ length ps
        Hackage -> return ()
        Clone -> repoAction cwd mdist ps (return ())
        Pull -> repoAction cwd mdist ps (cmd_ "git" ["pull", "--rebase"])
        Diff -> repoAction cwd mdist ps (cmd_ "git" ["diff"])
  where
    mode "clone" = Clone
    mode "pull" = Pull
    mode "list" = List
    mode "count" = Count
    mode "diff" = Diff
    mode "hackage" = Hackage
    mode _ = error "Unknown command"

commands :: [String]
commands = ["clone", "pull" , "list", "count", "diff", "hackage"]

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "CMD [DIST]\n"
    ++ "\n"
    ++ "Commands:\n"
    ++ "  clone\t- clone repos\n"
    ++ "  pull\t\t- pull repos\n"
    ++ "  list\t\t- list packages\n"
    ++ "  count\t\t- count number of packages\n"
    ++ "  hackage\t\t- generate Hackage distro date\n"
  exitWith (ExitFailure 1)

type Package = String

type Arguments = Maybe (String, Maybe Dist, [Package])

parseArgs :: [String] -> IO Arguments
parseArgs [c] = return (Just (c, Nothing, []))
parseArgs (c:dist:pkgs) | dist `notElem` dists = 
                          giveUp $ "Unknown dist '" ++ dist ++ "'"
                        | otherwise =
                          return $ Just (c, Just dist, pkgs)
  where
    giveUp :: String -> IO Arguments
    giveUp err = do
      hPutStrLn stderr err
      help >> return Nothing
parseArgs _ = help >> return Nothing

kojiListHaskell :: Bool -> Maybe Dist -> IO [Package]
kojiListHaskell verbose mdist = do
  when verbose $ putStrLn "Getting package list from Koji"
  libs <- filter ("ghc-" `isPrefixOf`) <$> kojiListPkgs (fromMaybe "rawhide" mdist)
  when (null libs) $ error "No library packages found"
  base <- cmd "ghc-pkg" ["--simple-output", "list", "base"]
  ghcver <- cmd "ghc" ["--numeric-version"]
  when verbose $ putStrLn "Getting packages from repoquery"
  bin <- words <$> cmd "dnf" ["repoquery", "--quiet", "--whatrequires", "libHS" ++ base ++ "-ghc" ++ ghcver ++ ".so()(64bit)", "--qf=%{source_name}"]
  return $ sort . nub $ bin ++ libs

repoAction :: FilePath -> Maybe Dist -> [Package] -> IO () -> IO ()
repoAction _ _ [] _ = return ()
repoAction topdir mdist (pkg:rest) action = do
  setCurrentDirectory topdir
  let branchGiven = isJust mdist
      branch = maybe "master" distBranch mdist
  putStrLn $ "\n==" +-+ pkg ++ (if branchGiven then ":" ++ branch else "") +-+ "=="
  -- muser <- getEnv "USER"
  -- let anon = "-a"
  dirExists <- doesDirectoryExist pkg
  unless dirExists $
    cmd_ "fedpkg" $ ["clone"] ++ (if branchGiven then ["-b ", branch] else ["-B"]) ++ [pkg]
  wd <- pkgDir pkg branch ""
  setCurrentDirectory wd
  pkggit <- do
    gd <- doesFileExist ".git/config"
    if gd 
      then cmdBool "grep" ["-q", "pkgs.fedoraproject.org", ".git/config"]
      else return False
  if not pkggit
    then error $ "not a Fedora pkg git dir!:" +-+ wd
    else do
      when dirExists $ do
        actual <- gitBranch
        when (branch /= actual) $
          cmd_ "fedpkg" ["switch-branch", branch]
        action
      let spec = pkg ++ ".spec"
      hasSpec <- doesFileExist spec
      unless hasSpec $ putStrLn "No spec file!"
      repoAction topdir mdist rest action

pkgDir :: String -> String -> FilePath -> IO FilePath
pkgDir dir branch top = do
  b <- doesDirectoryExist $ top </> dir </> branch
  return $ top </> dir </> if b then branch else ""

gitBranch :: IO String
gitBranch =
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["branch"]

