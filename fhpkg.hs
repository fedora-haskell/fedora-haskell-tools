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
import Control.Monad (unless, when)
import Data.Maybe
import Data.List (isInfixOf, isPrefixOf, nub, sort, (\\))

import System.Directory (doesDirectoryExist, doesFileExist,
                         setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
--import System.Posix.Env (getEnv)

import Dists (Dist, dists, distBranch, hackageRelease, releaseVersion)
import Koji (kojiListPkgs)
import RPM (rpmspec)
import Utils ((+-+), cmd, cmd_, cmdBool, cmdMaybe, maybeRemovePrefix,
              removePrefix, withCurrentDirectory)

main :: IO ()
main = do
  margs <- getArgs >>= parseArgs
  case margs of
    Nothing -> return ()
    Just (com, mdist, pkgs) ->
      case com of
        "list" -> withPackages mdist pkgs (mapM_ putStrLn)
        "count" -> withPackages mdist pkgs (print . length)
        "hackage" -> do
          let currentHackage = Just hackageRelease
          unless (isNothing mdist || mdist == currentHackage) $ error $ "Hackage is currently for" +-+ fromJust currentHackage ++ "!"
          withPackages currentHackage pkgs (repoqueryHackageCSV currentHackage)
        "clone" -> withPackages mdist pkgs $
                   repoAction_ True False mdist (return ())
        "clone-new" -> do
          new <- newPackages mdist
          withPackages mdist new $ repoAction_ True False mdist (return ())
        "pull" -> withPackages mdist pkgs $
                  repoAction_ True False mdist (cmd_ "git" ["pull", "--rebase"])
        "diff" -> withPackages mdist pkgs $
                  repoAction_ True False mdist (cmd_ "git" ["--no-pager", "diff"])
        "diff-branch" -> withPackages mdist pkgs $
                  repoAction False True mdist compareRawhide
        "diff-stackage" -> withPackages mdist pkgs $
                  repoAction True True mdist compareStackage
        "verrel" -> withPackages mdist pkgs $
                    repoAction_ False True mdist (cmd_ "fedpkg" ["verrel"])
        "subpkgs" -> withPackages mdist pkgs $
                     repoAction True True mdist (\ p -> rpmspec [] (Just "%{name}-%{version}") (p ++ ".spec") >>= putStrLn)
        "new" -> newPackages mdist >>= mapM_ putStrLn
        _ -> return ()
  where
    withPackages :: Maybe Dist -> [Package] -> ([Package] -> IO ()) -> IO ()
    withPackages mdist pkgs act =
      (if null pkgs then repoqueryHaskell False mdist else return pkgs) >>= act

commands :: [(String, String)]
commands = [("clone", "clone repos"),
            ("clone-new", "clone new packages"),
            ("count", "count number of packages"),
            ("diff", "git diff"),
            ("diff-branch","compare branch with master"),
            ("diff-stackage","compare with stackage"),
            ("hackage", "generate Hackage distro data"),
            ("list", "list packages"),
            ("new", "new unbuilt packages"),
            ("pull", "pull repos"),
            ("subpkgs", "list subpackages"),
            ("verrel", "show nvr of packages")]

cmds :: [String]
cmds = map fst commands

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "CMD [DIST]\n"
    ++ "\n"
    ++ "Commands:\n"
  mapM_ (putStrLn . (\(c, desc) -> "  " ++ c ++ replicate (mx - length c) ' ' +-+ "-" +-+ desc)) commands
  exitWith (ExitFailure 1)
  where
    mx = maximum $ map length cmds

type Package = String

type Arguments = Maybe (String, Maybe Dist, [Package])

parseArgs :: [String] -> IO Arguments
parseArgs [c] =
                if c `elem` cmds
                then return (Just (c, Nothing, []))
                else giveUp $ "No such command '" ++ c ++ "'"
parseArgs (c:dist:pkgs) | c `notElem` cmds =
                          giveUp $ "No such command '" ++ c ++ "'"
                        | dist `notElem` dists =
                            return $ Just (c, Nothing, dist:pkgs)
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
  init . unlines . sort . map (replace "\"ghc-" "\"")  . lines <$> cmd "dnf" (["repoquery", "--quiet", "--releasever=" ++ relver, "--latest-limit=1", "-q", "--qf=\"%{name}\",\"%{version}\",\"https://src.fedoraproject.org/rpms/%{name}\""] ++ pkgs) >>= putStr

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b s@(x:xs) =
  if a `isPrefixOf` s
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

newPackages :: Maybe Dist -> IO [Package]
newPackages mdist = do
  ps <- repoqueryHaskell True mdist
  kps <- kojiListHaskell True mdist
  return $ kps \\ ps

repoAction :: Bool -> Bool -> Maybe Dist -> (Package -> IO ()) -> [Package] -> IO ()
repoAction _ _ _ _ [] = return ()
repoAction header needsSpec mdist action (pkg:rest) = do
  withCurrentDirectory "." $ do
    let branchGiven = isJust mdist
        branch = maybe "master" distBranch mdist
    when header $
      putStrLn $ "\n==" +-+ pkg ++ (if branchGiven then ":" ++ branch else "") +-+ "=="
    -- muser <- getEnv "USER"
    -- let anon = "-a"
    dirExists <- doesDirectoryExist pkg
    unless dirExists $
      cmd_ "fedpkg" $ ["clone"] ++ (if branchGiven then ["-b", branch] else ["-B"]) ++ [pkg]
    singleDir <- doesFileExist $ pkg </> ".git/config"
    unless singleDir $ do
      branchDir <- doesDirectoryExist $ pkg </> branch
      unless branchDir $
        withCurrentDirectory pkg $
          cmd_ "fedpkg" ["clone", "-b", branch, pkg, branch]
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
        unless (needsSpec && not hasSpec) $
          action pkg
  repoAction header needsSpec mdist action rest

repoAction_ :: Bool -> Bool -> Maybe Dist -> IO () -> [Package] -> IO ()
repoAction_ header needsSpec mdist action =
  repoAction header needsSpec mdist (const action)

pkgDir :: String -> String -> FilePath -> IO FilePath
pkgDir dir branch top = do
  b <- doesDirectoryExist $ top </> dir </> branch
  return $ top </> dir </> if b then branch else ""

gitBranch :: IO String
gitBranch =
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["branch"]

compareStackage :: Package -> IO ()
compareStackage p = do
  nvr <- cmd "fedpkg" ["verrel"]
  stkg <- cmdMaybe "stackage" ["package", "lts", maybeRemovePrefix "ghc-" p]
  let same = isJust stkg && fromJust stkg `isInfixOf` nvr
  putStrLn $ removePrefix (p ++ "-") nvr +-+ "(fedora)"
  putStrLn $ (if same then "same" else fromMaybe "none" stkg) +-+ "(lts)"


compareRawhide :: Package -> IO ()
compareRawhide p = do
  nvr <- removeDisttag <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") (p ++ ".spec")
  nvr' <- withCurrentDirectory "../master" $
          removeDisttag <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") (p ++ ".spec")
  when (nvr /= nvr') $ do
    putStrLn nvr
    putStrLn nvr'
    putStrLn ""
  where
    removeDisttag = reverse . tail . dropWhile (/= '.') . reverse
