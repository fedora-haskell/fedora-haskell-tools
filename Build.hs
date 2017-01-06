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
import Control.Monad (filterM, unless, when)
import Data.Maybe
import Data.List (intercalate, isPrefixOf, nub)

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), dropExtension, takeBaseName, takeDirectory)
import System.IO (hPutStrLn, stderr)

import Dists
import RPM
import Utils

data Command = Install | Mock | Koji | Pending | Changed | Built deriving (Eq)

main :: IO ()
main = do
  (com:dist:pkgs, mdir) <- getArgs >>= parseArgs
  cwd <- getCurrentDirectory
  build cwd (mode com) dist mdir Nothing pkgs
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
parseArgs (c:_) | c `notElem` commands = giveUp $ "Unknown command '" ++ c ++ "'"
parseArgs [c] = do
  (dist, pkg, dir) <- determinePkgBranch
  return (c:dist:[pkg], Just dir)
parseArgs [_, d ] | d `elem` dists = giveUp "Please specify a package."
parseArgs [c, pkg] = do
  cwd <- getCurrentDirectory
  exists <- doesDirectoryExist pkg
  if exists then do
    setCurrentDirectory pkg
    (dist, pkg', dir) <- determinePkgBranch
    setCurrentDirectory cwd
    return (c:dist:[pkg'], Just dir)
    else return ([c, rawhide, pkg], Nothing)
parseArgs (c:dist:pkgs) | dist `notElem` dists = giveUp $ "Unknown dist '" ++ dist ++ "'"
                        | null pkgs = giveUp $ "Unknown dist '" ++ dist ++ "'"
                        | otherwise =
                            return (c:dist:map (removeSuffix "/") pkgs, Nothing)
parseArgs _ = help >> return ([], Nothing)

giveUp :: String -> IO ([String], Maybe FilePath)
giveUp err = do
  hPutStrLn stderr err
  help >> return ([], Nothing)

determinePkgBranch :: IO (String, String, FilePath) -- (branch, pkg, dir)
determinePkgBranch = do
  dir <- getCurrentDirectory
  let base = takeBaseName dir
  if base `elem` map distBranch dists
    then return (base, takeBaseName $ takeDirectory dir, dir)
    else do
    git <- doesDirectoryExist (dir </> ".git")
    if git
      then do
      branch <- gitBranch
      return (branch, base, dir)
      else
      error "Not a git repo: cannot determine branch"

build :: FilePath -> Command -> String -> Maybe FilePath -> Maybe String -> [String] -> IO ()
build _ _ _ _ _ [] = return ()
build topdir mode dist mdir mdep (pkg:rest) = do
  setCurrentDirectory topdir
  let dir = fromMaybe pkg mdir
      branch = distBranch dist
  dirExists <- if isJust mdir then return True else doesDirectoryExist dir
  unless (mode `elem` [Pending, Changed, Built]) $
    putStrLn $ "\n==" +-+ pkg ++ ":" ++ branch +-+ "=="
  unless dirExists $ do
    let anon = ["-a" | mode /= Koji]
    cmdlog "fedpkg" $ ["clone", "-b", branch, pkg] ++ anon
  wd <- pkgDir dir branch ""
  setCurrentDirectory wd
  retired <- doesFileExist "dead.package"
  if retired then
    when (mode `elem` [Install, Koji]) $ putStrLn "skipping dead.package"
    else do
    pkggit <- cmdBool "grep" ["-q", "pkgs.fedoraproject.org", ".git/config"]
    if not pkggit
      then when (mode `elem` [Install, Koji]) (error $ "not a Fedora pkg git dir!:" +-+ wd)
      else do
      when dirExists $ do
        actual <- gitBranch
        when (branch /= actual) $
          cmd_ "fedpkg" ["switch-branch", branch]
        cmd_ "git" ["pull", "-q"]
      nvr <- cmd "fedpkg" ["verrel"]
      let verrel = removePrefix (pkg ++ "-") nvr
          release = tail $ dropWhile (/= '-') verrel
          tag = distTag dist
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
              mapM_ (fhbuildMissing topdir dist) hmissing
              setCurrentDirectory $ topdir </> wd
            stillMissing <- map (uncurry maybePkgVer) <$> filterM notInstalled missing
            pkgmgr <- packageManager

            unless (null stillMissing) $ do
              putStrLn $ "Installing:" +-+ intercalate ", " stillMissing
              rpmInstall stillMissing
            putStrLn $ "Building" +-+ nvr +-+ "(buildlog:" +-+ wd </> ".build-" ++ verrel ++ ".log" ++ ")"
            -- note "fedpkg --path dir local" saves .build.log in cwd
            cmdlog "fedpkg" ["-q", "local"]
            opkgs <- lines <$> cmd "rpmspec" ["-q", "--queryformat", "%{name}\n", spec]
            rpms <- lines <$> cmd "rpmspec" ["-q", "--queryformat", "%{arch}/%{name}-%{version}-" ++ release ++ ".%{arch}.rpm\n", spec]
            built <- doesFileExist $ head rpms
            when built $
              putStrLn $ nvr +-+ "built\n"
            ipkgs <- lines <$> cmd "rpm" ("-qa":opkgs)
            unless (null ipkgs) $
              sudo pkgmgr ("--setopt=clean_requirements_on_remove=no":"remove":"-y":ipkgs)
            if built
              then do
              -- maybe filter out pandoc-pdf if not installed
              rpmInstall rpms
              setCurrentDirectory topdir
              else error $ "Build of " ++ nvr ++ " failed!"
        Mock -> do
          putStrLn $ "Mock building" +-+ nvr
          cmdlog "fedpkg" ["mockbuild"]
        Koji -> do
          cmd_ "git" ["--no-pager", "log", "-1"]
          putStrLn ""
          latest <- kojiLatestPkg tag pkg
          if nvr == latest
            then error $ nvr +-+ "already built!"
            else do
            putStrLn $ latest +-+ "->" +-+ nvr ++ "\n"
            kojiout <- cmd "fedpkg" $ ["build", "--nowait"] ++ maybe [] (\ d -> "--target":[d]) (distTarget dist)
            putStrLn kojiout
            let task = parseKojiBuild $ lines kojiout
            when (isNothing task)$ error "Could not parse koji task #!"
            -- FIXME poll if lose network connection
            cmdlog "koji" ["watch-task", fromJust task]
            logMsg $ nvr +-+ "built"
            when (distOverride dist) $ do
              user <- shell "grep Subject: ~/.fedora.cert | sed -e 's@.*CN=\\(.*\\)/emailAddress=.*@\\1@'"
              -- FIXME: improve Notes with recursive info
              cmd_ "bodhi" ["-o", nvr, "-u", user, "-N", "Haskell stack"]
            unless (null rest) $ do
              dep <- dependent pkg (head rest) branch topdir
              when dep $
                cmdlog "koji" ["wait-repo", tag, "--build=" ++ nvr]
              putStrLn ""
              putStrLn $ show (length rest) +-+ "packages left"
        Pending -> do
          latest <- kojiLatestPkg tag pkg
          unless (eqNVR nvr latest) $
            putStrLn $ latest +-+ "->" +-+ nvr
        Changed -> do
          latest <- kojiLatestPkg tag pkg
          unless (eqNVR nvr latest) $
            putStrLn pkg
        Built -> do
          latest <- kojiLatestPkg tag pkg
          when (eqNVR nvr latest) $
            putStrLn pkg
    build topdir mode dist mdir mdep rest

pkgDir :: String -> String -> FilePath -> IO FilePath
pkgDir dir branch top = do
  b <- doesDirectoryExist $ top </> dir </> branch
  return $ top </> dir </> if b then branch else ""

maybePkgVer :: String -> Maybe String -> String
maybePkgVer pkg mver = pkg ++ maybe "" ("-" ++) mver

notInstalled :: (String, Maybe String) -> IO Bool
notInstalled (pkg, mver) =
  not <$> cmdBool "rpm" ["--quiet", "-q", maybePkgVer pkg mver]

fhbuildMissing :: FilePath -> String -> String -> IO ()
fhbuildMissing topdir dist dep = do
  base <- derefSrcPkg dep
  maybe (error $ "No" +-+ dep +-+ "package available!")
    (\ p -> build topdir Install dist Nothing (Just dep) [p]) base

derefPkg :: (String, Maybe String) -> IO (String, Maybe String)
derefPkg (pkg, mver) = do
  res <- singleLine <$> repoquery ["--qf", "%{name}", "--whatprovides"] pkg
  when (null res) $ do
    installed <- not <$> notInstalled (pkg, mver)
    unless installed $ putStrLn $ "Warning:" +-+ pkg +-+ "not found by repoquery"
  return (if null res then pkg else res, mver)

derefSrcPkg :: String -> IO (Maybe String)
derefSrcPkg pkg = do
  res <- singleLine <$> repoquerySrc pkg
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

dependent :: String -> String -> String -> FilePath -> IO Bool
dependent dep pkg branch topdir = do
  pkgpath <- pkgDir pkg branch topdir
  cmdBool "grep" ["-q", dep, pkgpath </> pkg ++ ".spec"]

parseKojiBuild :: [String] -> Maybe String
parseKojiBuild [] = Nothing
parseKojiBuild (l:ls) | "Created task:" `isPrefixOf` l = Just $ removePrefix "Created task: " l
                      | otherwise = parseKojiBuild ls
