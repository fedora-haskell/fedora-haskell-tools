{-# LANGUAGE CPP #-}

-- |
-- Module      :  RPM
-- Copyright   :  (C) 2015-2019  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: RPM utils

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module RPM (buildRequires,
            derefSrcPkg,
            haskellSrcPkgs,
            Package,
            packageManager,
            repoquery,
            rqfnewline,
            rpmInstall,
            rpmspec) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)
import Data.List (isPrefixOf, isSuffixOf, nub, (\\))
import Data.Maybe (isJust, isNothing)
import Distribution.Fedora.Branch (Branch, branchRelease, showBranch)
import Distribution.Fedora.Release (releaseVersion)
import SimpleCmd (cmd, cmdN, removeStrictPrefix, removeSuffix, sudo_, warning, (+-+))
import qualified SimpleCmd.Rpm as S
import System.Directory (doesDirectoryExist, findExecutable)
import System.FilePath ((</>), takeDirectory)
import System.Exit (ExitCode (..), exitFailure, exitWith)

-- @since base 4.8.0.0
die :: String -> IO a
die err = warning err >> exitFailure

requireProgram :: String -> IO ()
requireProgram c = do
  mavail <- findExecutable c
  when (isNothing mavail) $ die (c ++ ": command not found")

optionalProgram :: String -> IO Bool
optionalProgram c = isJust <$> findExecutable c

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
  sudo_ inst $ ["-y", "--nogpgcheck", arg] ++ rpms

-- should be \n for dnf5 repoquery workaround
-- FIXME integrate into repoquery or add to SimpleCmd.Rpm?
rqfnewline :: String
rqfnewline = "\n"

-- FIXME use frpq
repoquery :: Branch -> [String] -> IO String
repoquery branch args = do
  havednf <- optionalProgram "dnf"
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "--quiet"]) else ("repoquery", [])
  rel <- branchRelease branch
  let releasever = ["--releasever=" ++ releaseVersion rel]
  -- FIXME debug output
  when False $
    cmdN prog (subcmd ++ releasever ++ args)
  cmd prog (subcmd ++ releasever ++ args)

repoquerySrc :: Branch -> String -> IO (Maybe String)
repoquerySrc branch key = do
  havednf <- optionalProgram "dnf"
  let srcflag = if havednf then ["--qf=%{source_name}"] else ["--qf", "%{base_package_name}"]
  let repo = ["--repo=koji-buildroot", "--repofrompath", "koji-buildroot,https://kojipkgs.fedoraproject.org/repos" </> showBranch branch </> "latest/x86_64/"]
  res <- words <$> repoquery branch (srcflag ++ repo ++ ["--whatprovides", key])
  return $ case res of
    [p] -> Just p
    ps | key `elem` ps -> Just key
    _ -> Nothing

rpmspec :: [String] -> Maybe String -> FilePath -> IO [String]
rpmspec args =
  S.rpmspec (["--define", "ghc_version any"] ++ args)

buildRequires :: FilePath -> IO [String]
buildRequires spec =
  -- FIXME should resolve "pkgconfig()" etc
  map (head . words) <$> rpmspec ["--buildrequires"] Nothing spec
--    >>= mapM (whatProvides relver)

type Package = String

derefSrcPkg :: FilePath -> Branch -> Bool -> Package -> IO Package
derefSrcPkg topdir dist verb pkg = do
  res <- maybeDerefSrcPkg topdir dist verb pkg
  case res of
    Nothing -> do
      putStrLn $ "Unknown package" +-+ removeSuffix "-devel" pkg
      exitWith (ExitFailure 1)
    Just s -> do
      when (pkg /= s && verb) $ putStrLn $ pkg +-+ "->" +-+ s
      return s

derefSrcPkgRelax :: FilePath -> Branch -> Package -> IO Package
derefSrcPkgRelax topdir dist pkg = do
  res <- maybeDerefSrcPkg topdir dist False pkg
  case res of
    Nothing -> return pkg
    Just s -> return s

maybeDerefSrcPkg :: FilePath -> Branch -> Bool -> Package -> IO (Maybe Package)
maybeDerefSrcPkg topdir dist verb pkg = do
  let lib = removeLibSuffix pkg
  -- fixme: should check branch (dir)
  libExists <- doesDirectoryExist $ topdir </> lib
  if libExists
    then return $ Just lib
    else do
    -- todo: check bin has lib
    let bin = removeStrictPrefix "ghc-" lib
    binExists <- doesDirectoryExist $ topdir </> bin
    if binExists
      then return $ Just bin
      else do
      when verb $ putStrLn $ "Repoquerying" +-+ pkg
      repoquerySrc dist pkg

isHaskellDevelPkg :: Package -> Bool
isHaskellDevelPkg pkg = "ghc-" `isPrefixOf` pkg && ("-devel" `isSuffixOf` pkg || "-prof" `isSuffixOf` pkg || "-static" `isSuffixOf` pkg) || pkg `elem`haskellTools

removeLibSuffix :: String -> String
removeLibSuffix p | "-devel" `isSuffixOf` p = removeSuffix "-devel" p
                  | "-prof" `isSuffixOf` p = removeSuffix "-prof" p
                  | "-static" `isSuffixOf` p = removeSuffix "-static" p
                  | otherwise = p

haskellTools :: [Package]
haskellTools = ["alex", "cabal-install", "gtk2hs-buildtools", "happy", "hspec-discover"]

haskellSrcPkgs ::  FilePath -> Branch -> [Package] -> IO [Package]
haskellSrcPkgs topdir branch brs = do
  ghcLibs <- do
    let ghcDir = takeDirectory topdir </> "ghc"
    map removeLibSuffix . filter isHaskellDevelPkg <$> rpmspec [] (Just "%{name}\n") (ghcDir </> "ghc.spec")
  let hdeps = filter (\ dp -> "ghc-" `isPrefixOf` dp || dp `elem` haskellTools) (map removeLibSuffix brs \\ (["ghc-rpm-macros", "ghc-rpm-macros-extra"] ++ ghcLibs))
  nub <$> mapM (derefSrcPkgRelax topdir branch) hdeps
