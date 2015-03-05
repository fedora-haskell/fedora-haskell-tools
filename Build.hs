-- |
-- Module      :  Build
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: Main entry point for building RPM packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (filterM, unless, when)
import Data.Maybe (fromMaybe, isJust)
import Data.List (intercalate, isPrefixOf, nub)

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), dropExtension, takeBaseName, takeDirectory)
import System.IO (hPutStrLn, stderr)

import Dists
import Utils

data Command = Install | Mock | Koji | Pending | Changed | Built deriving (Eq)

main :: IO ()
main = do
  (com:dist:pkgs, mdir) <- getArgs >>= parseArgs
  build (mode com) dist mdir Nothing pkgs
  where
    mode "install" = Install
    mode "mock" = Mock
    mode "koji" = Koji
    mode "pending" = Pending
    mode "changed" = Changed
    mode "built" = Built
    mode _ = undefined

commands :: [String]
commands = ["install", "mock" , "koji", "pending", "changed", "built"]

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "CMD [dist] [pkg] ...\n"
    ++ "\n"
    ++ "Commands:\n"
    ++ "  install\t- build locally and install\n"
    ++ "  mock\t\t- build in mock\n"
    ++ "  koji\t\t- build in Koji\n"
    ++ "  pending\t- show planned changes\n"
    ++ "  changed\t- show changed pkgs\n"
    ++ "  built\t\t- show pkgs whose NVR already built\n"
  exitWith (ExitFailure 1)

-- allow "fhbuild CMD", "fhbuild CMD dir" or "fhbuild CMD DIST PKG..."
parseArgs :: [String] -> IO ([String], Maybe FilePath)
parseArgs [c] | c `elem` commands = do
  (dist:pkgs, dir) <- determinePkgBranch
  return (c:dist:pkgs, Just dir)
parseArgs [c, pkg] | c `elem` commands = do
  cwd <- getCurrentDirectory
  exists <- doesDirectoryExist pkg
  if exists then do
    setCurrentDirectory pkg
    (dist:pkgs, dir) <- determinePkgBranch
    setCurrentDirectory cwd
    return (c:dist:pkgs, Just dir)
    else return ([c, rawhide, pkg], Nothing)
parseArgs (c:dist:pkgs) |  c `elem` commands
                           && dist `elem` dists
                           && not (null pkgs) =
                             return (c:dist:pkgs, Nothing)
parseArgs _ = help >> return ([], Nothing)

determinePkgBranch :: IO ([String], FilePath) -- (branch:pkgs, dir)
determinePkgBranch = do
  dir <- getCurrentDirectory
  let base = takeBaseName dir
  if base `elem` ["master", "f20", "f19"]
    then return ([base, takeBaseName $ takeDirectory dir], dir)
    else do
    git <- doesDirectoryExist (dir </> ".git")
    if git
      then do
      branch <- gitBranch
      return ([branch, base], dir)
      else
      error "Not a git repo: cannot determine branch"

build :: Command -> String -> Maybe FilePath -> Maybe String -> [String] -> IO ()
build _ _ _ _ [] = return ()
build mode dist mdir mdep (pkg:rest) = do
  let dir = fromMaybe pkg mdir
      branch = distBranch dist
  dirExists <- if isJust mdir then return True else doesDirectoryExist dir
  unless (mode `elem` [Pending, Changed, Built]) $
    putStrLn $ "\n==" +-+ pkg ++ ":" ++ branch +-+ "=="
  unless dirExists $ do
    let anon = ["-a" | mode /= Koji]
    cmdlog "fedpkg" $ ["clone", "-b", branch, pkg] ++ anon
  b <- doesDirectoryExist $ dir </> branch
  let wd = dir </> if b then branch else ""
  withCurrentDirectory wd $ \ cwd -> do
    retired <- doesFileExist "dead.package"
    unless retired $ do
      cmdAssert "not a Fedora pkg git dir!" "grep" ["-q", "pkgs.fedoraproject.org", ".git/config"]
      when dirExists $ do
        actual <- gitBranch
        when (branch /= actual) $
          cmd_ "fedpkg" ["switch-branch", branch]
        cmd_ "git" ["pull", "-q"]
      nvr <- cmd "fedpkg" ["verrel"]
      let verrel = removePrefix (pkg ++ "-") nvr
          target = distTarget dist
      case mode of
        Install -> do
          let req = fromMaybe pkg mdep
          installed <- cmdMaybe "rpm" ["-q", "--qf", "%{name}-%{version}-%{release}", req]
          if Just (req ++ "-" ++ verrel) == installed
            then putStrLn $ nvr +-+ "already installed!\n"
            else do
            putStrLn $ fromMaybe "Not installed" installed +-+ "->" +-+ nvr
            cmd_ "git" ["--no-pager", "log", "-1"]
            putStrLn ""
            let spec = pkg ++ ".spec"
            putStrLn "repoquerying deps..."
            -- "pkg = X.Y" -> ["pkg", "=", "X.Y"] -> "pkg"
            depvers <- (map (processDeps . words) . lines) <$> cmd "rpmspec" ["-q", "--buildrequires", spec] >>= mapM derefPkg
            missing <- nub <$> filterM notInstalled depvers
            -- FIXME sort into build order
            let hmissing = filter (\ dp -> "ghc-" `isPrefixOf` dp || dp `elem` ["alex", "cabal-install", "gtk2hs-buildtools", "happy"]) (map fst missing)
            unless (null hmissing) $ do
              putStrLn "Missing:"
              mapM_ putStrLn hmissing
              withCurrentDirectory cwd $ \ _ ->
                mapM_ (fhbuildMissing dist) hmissing
            stillMissing <- map (uncurry maybePkgVer) <$> filterM notInstalled missing
            unless (null stillMissing) $ do
              putStrLn $ "Installing:" +-+ intercalate ", " stillMissing
              sudo "yum" $ ["install", "-y", "--nogpgcheck"] ++ stillMissing
            putStrLn $ "Building" +-+ nvr +-+ "(buildlog:" +-+ wd </> ".build-" ++ verrel ++ ".log" ++ ")"
            -- note "fedpkg --path dir local" saves .build.log in cwd
            cmdlog "fedpkg" ["-q", "local"]
            putStrLn $ nvr +-+ "built\n"
            opkgs <- lines <$> cmd "rpmspec" ["-q", "--queryformat", "%{name}\n", spec]
            ipkgs <- lines <$> cmd "rpm" ("-qa":opkgs)
            unless (null ipkgs) $
              sudo "yum" ("remove":ipkgs)
            arch <- cmd "arch" []
            -- maybe filter out pandoc-pdf if not installed
            let rpms = map (\ p -> arch </> p ++ "-" ++ verrel ++ "." ++ arch ++ ".rpm") opkgs
            sudo "yum" $ ["-y", "localinstall"] ++ rpms
        Mock -> do
          putStrLn $ "Mock building" +-+ nvr
          cmdlog "fedpkg" ["mockbuild"]
        Koji -> do
          cmd_ "git" ["--no-pager", "log", "-1"]
          putStrLn ""
          latest <- kojiLatestPkg target pkg
          if nvr == latest
            then error $ nvr +-+ "already built!"
            else do
            putStrLn $ latest +-+ "->" +-+ nvr ++ "\n"
            cmdlog "fedpkg" $ ["build"] ++ (maybe [] (["--target"] ++) (distBuildTarget dist))
            when (distOverride dist) $ do
              user <- shell "grep Subject: ~/.fedora.cert | sed -e 's@.*CN=\\(.*\\)/emailAddress=.*@\\1@'"
              -- FIXME: improve Notes with recursive info
              cmdlog "bodhi" ["-o", nvr, "-u", user, "-N", pkg +-+ "stack"]
            when (not (null rest)) $ do
               dep <- dependent pkg (head rest)
               when dep $
                 cmdlog "koji" ["wait-repo", target, "--build=" ++ nvr]
        Pending -> do
          latest <- kojiLatestPkg target pkg
          unless (eqNVR nvr latest) $
            putStrLn $ latest +-+ "->" +-+ nvr
        Changed -> do
          latest <- kojiLatestPkg target pkg
          unless (eqNVR nvr latest) $
            putStrLn pkg
        Built -> do
          latest <- kojiLatestPkg target pkg
          when (eqNVR nvr latest) $
            putStrLn pkg
      build mode dist mdir mdep rest

withCurrentDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withCurrentDirectory dir =
    bracket
        (do cwd <- getCurrentDirectory
            exists <- doesDirectoryExist dir
            if exists
              then setCurrentDirectory dir
              else error $ "Cannot set non-existent directory" +-+ dir
            return cwd)
        setCurrentDirectory

maybePkgVer :: String -> Maybe String -> String
maybePkgVer pkg mver = pkg ++ maybe "" ("-" ++) mver

notInstalled :: (String, Maybe String) -> IO Bool
notInstalled (pkg, mver) =
  not <$> cmdBool "rpm" ["--quiet", "-q", maybePkgVer pkg mver]

fhbuildMissing :: String -> String -> IO ()
fhbuildMissing dist dep = do
  base <- derefSrcPkg dep
  maybe (error $ "No" +-+ dep +-+ "package available!")
    (\ p -> build Install dist Nothing (Just dep) [p]) base

derefPkg :: (String, Maybe String) -> IO (String, Maybe String)
derefPkg (pkg, mver) = do
  res <- singleLine <$> cmd "repoquery" ["--qf", "%{name}", "--whatprovides", pkg]
  when (null res) $ do
    installed <- not <$> notInstalled (pkg, mver)
    unless installed $ putStrLn $ "Warning:" +-+ pkg +-+ "not found by repoquery"
  return (if null res then pkg else res, mver)

derefSrcPkg :: String -> IO (Maybe String)
derefSrcPkg pkg = do
  res <- singleLine <$> cmd "repoquery" ["--qf", "%{base_package_name}", "--whatprovides", pkg]
  if null res
     -- maybe package has never been built yet
    then do
    let nondevel = removeSuffix "-devel" pkg
    p <- pkgdb nondevel
    if isJust p
      then return p
      else pkgdb $ removePrefix "ghc-" nondevel
    else return $ Just res

gitBranch :: IO String
gitBranch =
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["branch"]

pkgdb :: String -> IO (Maybe String)
pkgdb pkg = do
  res <- words <$> shell ("pkgdb-cli list --nameonly" +-+ pkg +-+ "| grep" +-+ pkg ++ "$")
  return $ if pkg `elem` res then Just pkg else Nothing

eqNVR :: String -> String -> Bool
eqNVR p1 p2 =
  dropExtension p1 == dropExtension p2

processDeps :: [String] -> (String, Maybe String)
processDeps [p, "=", v] = (p, Just v)
processDeps (p:_) = (p, Nothing)
processDeps [] = error "processDeps: empty string!"

-- FIXME
dependent :: String -> String -> IO Bool
dependent dep pkg = return True
