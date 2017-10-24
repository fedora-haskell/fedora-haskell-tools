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
-- cache dist package lists?
-- push subpackages to Hackage
-- compare branch versions
-- compare with LTS
-- arbitrary command
-- query-format string

module Main where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Maybe
import Data.List (isPrefixOf, nub, sort, (\\))

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
--import System.Posix.Env (getEnv)

import Dists (Dist, dists, distBranch, releaseVersion)
import Koji (kojiListPkgs)
import Utils ((+-+), cmd, cmd_, cmdBool, removePrefix)

main :: IO ()
main = do
  margs <- getArgs >>= parseArgs
  case margs of
    Nothing -> return ()
    Just (com, mdist, pkgs) -> do
      ps <- if null pkgs then repoqueryHaskell False mdist else return pkgs
      case com of
        "list" -> mapM_ putStrLn ps
        "count" -> print $ length ps
        "hackage" -> do
          unless (mdist == Just "f27") $ error "Hackage is currently for F27!"
          repoqueryHackageCSV mdist ps
        "clone" -> do
          new <- newPackages mdist ps
          repoAction True mdist (new ++ ps) (return ())
        "pull" -> repoAction True mdist ps (cmd_ "git" ["pull", "--rebase"])
        "diff" -> repoAction True mdist ps (cmd_ "git" ["diff"])
        "verrel" -> repoAction False mdist ps (cmd_ "fedpkg" ["verrel"])
        "subpkgs" -> repoAction True mdist ps (cmd "fedpkg" ["gimmespec"] >>= \ p -> cmd_ "rpmspec" ["-q", "--qf", "%{name}-%{version}\n", p])
        "new" -> newPackages mdist ps >>= mapM_ putStrLn
        _ -> return ()

commands :: [String]
commands = ["clone", "count", "diff", "hackage", "list", "new", "pull" , "subpkgs", "verrel"]

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
    ++ "  verrel\t\t- show nvr of packages\n"
    ++ "  subpkgs\t\t- list subpackages\n"
    ++ "  new\t\t- new unbuilt packages\n"
    ++ "  hackage\t- generate Hackage distro date\n"
  exitWith (ExitFailure 1)

type Package = String

type Arguments = Maybe (String, Maybe Dist, [Package])

parseArgs :: [String] -> IO Arguments
parseArgs [c] =
                if c `elem` commands
                then return (Just (c, Nothing, []))
                else giveUp $ "No such command '" ++ c ++ "'"
parseArgs (c:dist:pkgs) | dist `notElem` dists = 
                          giveUp $ "Unknown dist '" ++ dist ++ "'"
                        | c `notElem` commands =
                          giveUp $ "No such command '" ++ c ++ "'"
                        | otherwise =
                          return $ Just (c, Just dist, pkgs)
parseArgs _ = help >> return Nothing

giveUp :: String -> IO Arguments
giveUp err = do
  hPutStrLn stderr err
  help >> return Nothing


kojiListHaskell :: Bool -> Maybe Dist -> IO [Package]
kojiListHaskell verbose mdist = do
  when verbose $ putStrLn "Getting package list from Koji"
  libs <- filter (\ p -> "ghc-" `isPrefixOf` p && p `notElem` ["ghc-rpm-macros", "ghc-srpm-macros"]) <$> kojiListPkgs (fromMaybe "rawhide" mdist)
  when (null libs) $ error "No library packages found"
  return $ sort $ nub libs

repoqueryHackageCSV :: Maybe Dist -> [Package] -> IO ()
repoqueryHackageCSV mdist pkgs = do
  let relver = maybe "rawhide" releaseVersion mdist
  -- Hackage csv chokes on final newline so remove it
  init . unlines . map (replace "\"ghc-" "\"")  . lines <$> (cmd "dnf" $ ["repoquery", "--quiet", "--releasever=" ++ relver, "-q", "--qf=\"%{name}\",\"%{version}\",\"https://apps.fedoraproject.org/packages/%{name}\""] ++ pkgs) >>= putStr

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b s@(x:xs) =
  if isPrefixOf a s
  then b ++ replace a b (drop (length a) s)
  else x:replace a b xs
replace _ _ [] = []

repoqueryHaskell :: Bool -> Maybe Dist -> IO [Package]
repoqueryHaskell verbose mdist = do
  -- fixme: should use repoquery instead:
  base <- cmd "ghc-pkg" ["--simple-output", "list", "base"]
  ghcver <- cmd "ghc" ["--numeric-version"]
  let relver = maybe "rawhide" releaseVersion mdist
  when verbose $ putStrLn "Getting packages from repoquery"
  bin <- words <$> cmd "dnf" ["repoquery", "--quiet", "--releasever=" ++ relver, "--whatrequires", "libHS" ++ base ++ "-ghc" ++ ghcver ++ ".so()(64bit)", "--qf=%{source_name}"]
  when (null bin) $ error "No libHSbase consumers found!"
  return $ sort $ nub bin

newPackages :: Maybe Dist -> [Package] -> IO [Package]
newPackages mdist ps = do
  kps <- kojiListHaskell True mdist
  return $ kps \\ ps

repoAction :: Bool -> Maybe Dist -> [Package] -> IO () -> IO ()
repoAction _ _ [] _ = return ()
repoAction header mdist (pkg:rest) action = do
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    let branchGiven = isJust mdist
        branch = maybe "master" distBranch mdist
    when header $
      putStrLn $ "\n==" +-+ pkg ++ (if branchGiven then ":" ++ branch else "") +-+ "=="
    -- muser <- getEnv "USER"
    -- let anon = "-a"
    dirExists <- doesDirectoryExist pkg
    unless dirExists $
      cmd_ "fedpkg" $ ["clone"] ++ (if branchGiven then ["-b", branch] else ["-B"]) ++ [pkg]
    wd <- pkgDir pkg branch ""
    setCurrentDirectory wd
    pkggit <- do
      gd <- doesFileExist ".git/config"
      if gd
        then cmdBool "grep" ["-q", "pkgs.fedoraproject.org", ".git/config"]
        else return False
    unless pkggit $
      error $ "not a Fedora pkg git dir!:" +-+ wd
    when dirExists $ do
      actual <- gitBranch
      when (branch /= actual) $
        cmd_ "fedpkg" ["switch-branch", branch]
      isDead <- doesFileExist "dead.package"
      unless isDead $ do
        let spec = pkg ++ ".spec"
        hasSpec <- doesFileExist spec
        unless hasSpec $ putStrLn "No spec file!"
        action
  repoAction header mdist rest action

pkgDir :: String -> String -> FilePath -> IO FilePath
pkgDir dir branch top = do
  b <- doesDirectoryExist $ top </> dir </> branch
  return $ top </> dir </> if b then branch else ""

gitBranch :: IO String
gitBranch =
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["branch"]

