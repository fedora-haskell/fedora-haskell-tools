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
            pkgDir,
            repoquery,
            rpmInstall,
            rpmspec) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)
import Data.List (isPrefixOf, isSuffixOf, nub, (\\))
import Data.Maybe (isJust, isNothing)
import System.Directory (doesDirectoryExist, findExecutable)
import System.FilePath ((</>))
-- die is available in ghc-7.10 base-4.8
import System.Exit (ExitCode (..), exitFailure, exitWith)

import Dist (distTag)
import FedoraDists (Dist, distBranch, dists, releaseVersion)
import SimpleCmd (cmd, removeStrictPrefix, removeSuffix, sudo_, warning, (+-+))
import qualified SimpleCmd.Rpm as S

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

repoquery :: Dist -> [String] -> IO String
repoquery dist args = do
  havednf <- optionalProgram "dnf"
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "--quiet"]) else ("repoquery", [])
      releasever = ["--releasever=" ++ releaseVersion dist]
  cmd prog (subcmd ++ releasever ++ args)

repoquerySrc :: Dist -> String -> IO (Maybe String)
repoquerySrc dist key = do
  havednf <- optionalProgram "dnf"
  let srcflag = if havednf then ["--qf=%{source_name}"] else ["--qf", "%{base_package_name}"]
      repo = if dist `elem` dists
             then ["--repo=koji-buildroot", "--repofrompath", "koji-buildroot,https://kojipkgs.fedoraproject.org/repos" </> distTag dist </> "latest/x86_64/"]
             else []
  res <- words <$> repoquery dist (srcflag ++ repo ++ ["--whatprovides", key])
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

derefSrcPkg :: FilePath -> Dist -> Bool -> Package -> IO Package
derefSrcPkg topdir dist verb pkg = do
  let lib = removeLibSuffix pkg
  -- fixme: should check branch (dir)
  libExists <- doesDirectoryExist $ topdir </> lib
  if libExists
    then return lib
    else do
    -- todo: check bin has lib
    let bin = removeStrictPrefix "ghc-" lib
    binExists <- doesDirectoryExist $ topdir </> bin
    if binExists
      then return bin
      else do
      putStrLn $ "Repoquerying" +-+ pkg
      res <- repoquerySrc dist pkg
      case res of
        Nothing -> do
          putStrLn $ "Unknown package" +-+ removeSuffix "-devel" pkg
          exitWith (ExitFailure 1)
        Just s -> do
          when (pkg /= s && verb) $ putStrLn $ pkg +-+ "->" +-+ s
          return s

isHaskellDevelPkg :: Package -> Bool
isHaskellDevelPkg pkg = "ghc-" `isPrefixOf` pkg && ("-devel" `isSuffixOf` pkg || "-prof" `isSuffixOf` pkg || "-static" `isSuffixOf` pkg) || pkg `elem`haskellTools

removeLibSuffix :: String -> String
removeLibSuffix p | "-devel" `isSuffixOf` p = removeSuffix "-devel" p
                  | "-prof" `isSuffixOf` p = removeSuffix "-prof" p
                  | "-static" `isSuffixOf` p = removeSuffix "-static" p
                  | otherwise = p

haskellTools :: [Package]
haskellTools = ["alex", "cabal-install", "gtk2hs-buildtools", "happy", "hspec-discover"]

haskellSrcPkgs ::  FilePath -> Dist -> [Package] -> IO [Package]
haskellSrcPkgs topdir dist brs = do
  ghcLibs <- do
    let branch = distBranch dist
    ghcDir <- pkgDir "ghc" branch (topdir </> "..")
    map removeLibSuffix . filter isHaskellDevelPkg <$> rpmspec [] (Just "%{name}") (ghcDir </> "ghc.spec")
  let hdeps = filter (\ dp -> "ghc-" `isPrefixOf` dp || dp `elem` haskellTools) (map removeLibSuffix brs \\ (["ghc-rpm-macros", "ghc-rpm-macros-extra"] ++ ghcLibs))
  nub <$> mapM (derefSrcPkg topdir dist False) hdeps

pkgDir :: String -> String -> FilePath -> IO FilePath
pkgDir dir branch top = do
  b <- doesDirectoryExist $ top </> dir </> branch
  return $ top </> dir </> if b then branch else ""
