-- |
-- Module      :  RPM
-- Copyright   :  (C) 2015-2017  Jens Petersen
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

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
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
import System.IO (hPutStrLn, stderr)

import Dists (Dist, distBranch, distTag, releaseVersion)
import Utils (cmd, removeSuffix, sudo, (+-+))

-- @since base 4.8.0.0
die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

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
  sudo inst $ ["-y", "--nogpgcheck", arg] ++ rpms

repoquery :: String -> [String] -> IO String
repoquery relver args = do
  havednf <- optionalProgram "dnf"
  let (prog, subcmd) = if havednf then ("dnf", ["repoquery", "--quiet"]) else ("repoquery", [])
  cmd prog (subcmd ++ ["--releasever=" ++ relver] ++ args)

repoquerySrc :: Dist -> String -> IO (Maybe String)
repoquerySrc dist key = do
  havednf <- optionalProgram "dnf"
  let srcflag = if havednf then ["--qf=%{source_name}"] else ["--qf", "%{base_package_name}"]
      relver = releaseVersion dist
  res <- words <$> repoquery relver (srcflag ++ ["--repofrompath", "koji-buildroot,http://kojipkgs.fedoraproject.org/repos" </> distTag dist </> "latest/x86_64/", "--whatprovides", key])
  return $ case res of
    [p] -> Just p
    ps | key `elem` ps -> Just key
    _ -> Nothing

rpmspec :: [String] -> Maybe String -> FilePath -> IO String
rpmspec args mqf spec = do
  let qf = maybe [] (\ q -> ["--queryformat", q]) mqf
  cmd "rpmspec" (["-q"] ++ args ++ qf ++ [spec])

buildRequires :: FilePath -> IO [String]
buildRequires spec =
  -- FIXME should resolve "pkgconfig()" etc
  map (head . words) . lines <$> rpmspec ["--buildrequires"] Nothing spec
--    >>= mapM (whatProvides relver)

type Package = String

derefSrcPkg :: FilePath -> Dist -> Package -> IO Package
derefSrcPkg topdir dist pkg =
  if isHaskellDevelPkg pkg
  then
    do let base = removeSuffix "-devel" pkg
       dirExists <- doesDirectoryExist $ topdir </> base
       if dirExists then return base else derefSrcPkg topdir dist base
  else
    do putStrLn $ "Repoquerying" +-+ pkg
       res <- repoquerySrc dist pkg
       case res of
         Nothing ->
           do putStrLn $ "Unknown package" +-+ removeSuffix "-devel" pkg
              exitWith (ExitFailure 1)
         Just s -> do
           when (pkg /= s) $ putStrLn $ pkg +-+ "->" +-+ s
           return s

isHaskellDevelPkg :: Package -> Bool
isHaskellDevelPkg pkg = "ghc-" `isPrefixOf` pkg && ("-devel" `isSuffixOf` pkg)

haskellSrcPkgs ::  FilePath -> String -> [Package] -> IO [Package]
haskellSrcPkgs topdir dist brs = do
  ghcLibs <- do
    let branch = distBranch dist
    ghcDir <- pkgDir "ghc" branch topdir
    filter isHaskellDevelPkg . words <$> rpmspec [] (Just "%{name}\n") (ghcDir </> "ghc.spec")
  let hdeps = filter (\ dp -> "ghc-" `isPrefixOf` dp || dp `elem` ["alex", "cabal-install", "gtk2hs-buildtools", "happy"]) (brs \\ (["ghc-rpm-macros", "ghc-rpm-macros-extra"] ++ ghcLibs))
  nub <$> mapM (derefSrcPkg topdir dist) hdeps

pkgDir :: String -> String -> FilePath -> IO FilePath
pkgDir dir branch top = do
  b <- doesDirectoryExist $ top </> dir </> branch
  return $ top </> dir </> if b then branch else ""
