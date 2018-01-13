-- |
-- Module      :  Build
-- Copyright   :  (C) 2014-2017  Jens Petersen
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

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, unless, when)
import Data.Maybe
import Data.List (intercalate, isPrefixOf, nub, (\\))

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), dropExtension)
import System.IO (hPutStrLn, stderr)

import Dists (Dist, dists, distBranch, distOverride, distTag, distTarget,
              releaseVersion)
import Koji (kojiBuilding, kojiLatestPkg, kojiWaitPkg, notInKoji)
import RPM (packageManager, rpmInstall, repoquerySrc, rpmspec)
import Utils ((+-+), checkFedoraPkgGit, cmd, cmd_, cmdBool, cmdMaybe, cmdlog,
              logMsg, removePrefix, removeSuffix, sudo)

data Command = Install | Mock | Koji | Chain | Pending | Changed | Built deriving (Eq)

main :: IO ()
main = do
  margs <- getArgs >>= parseArgs
  case margs of
    Nothing -> return ()
    Just (com, dist, pkgs) -> do
      cwd <- getCurrentDirectory
      build cwd (mode com) dist Nothing Nothing False pkgs
  where
    mode "install" = Install
    mode "mock" = Mock
    mode "koji" = Koji
    mode "chain" = Chain
    mode "pending" = Pending
    mode "changed" = Changed
    mode "built" = Built
    mode _ = error "Unknown command"

commands :: [String]
commands = ["install", "mock" , "koji", "chain", "pending", "changed", "built"]

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "CMD DIST PKG...\n"
    ++ "\n"
    ++ "Commands:\n"
    ++ "  install\t- build locally and install\n"
    ++ "  mock\t\t- build in mock\n"
    ++ "  koji\t\t- build in Koji\n"
    ++ "  chain\t\t- build deps recursively in Koji\n"
    ++ "  pending\t- show planned changes\n"
    ++ "  changed\t- show changed pkgs\n"
    ++ "  built\t\t- show pkgs whose NVR already built\n"
  exitWith (ExitFailure 1)

type Arguments = Maybe (String, Dist, [String])

parseArgs :: [String] -> IO Arguments
parseArgs (c:dist:pkgs) | dist `notElem` dists = giveUp $ "Unknown dist '" ++ dist ++ "'"
                        | null pkgs = giveUp "Please specify a package."
                        | otherwise =
                            return $ Just (c, dist, map (removeSuffix "/") pkgs)
parseArgs _ = help >> return Nothing

giveUp :: String -> IO Arguments
giveUp err = do
  hPutStrLn stderr err
  help >> return Nothing

build :: FilePath -> Command -> Dist -> Maybe String -> Maybe (String, String) -> Bool -> [String] -> IO ()
build _ _ _ _ _ _ [] = return ()
build topdir mode dist msubpkg mlast waitrepo (pkg:rest) = do
  setCurrentDirectory topdir
  let branch = distBranch dist
  unless (mode `elem` [Pending, Changed, Built]) $
    putStrLn $ "\n==" +-+ pkg ++ ":" ++ branch +-+ "=="
  dirExists <- doesDirectoryExist pkg
  unless dirExists $ do
    let anon = ["-a" | mode `notElem` [Koji, Chain]]
    cmdlog "fedpkg" $ ["clone", "-b", branch, pkg] ++ anon
  wd <- pkgDir pkg branch ""
  setCurrentDirectory wd
  retired <- doesFileExist "dead.package"
  if retired then do
    unless (mode == Pending) $
      putStrLn "skipping dead.package"
    build topdir mode dist Nothing Nothing False rest
    else do
    pkggit <- do
      gd <- doesFileExist ".git/config"
      if gd then checkFedoraPkgGit
        else return False
    if not pkggit
      then if mode `elem` [Install, Koji, Chain]
           then error $ "not a Fedora pkg git dir!:" +-+ wd
           else build topdir mode dist Nothing Nothing False rest
      else do
      when dirExists $ do
        actual <- gitBranch
        when (branch /= actual) $
          cmd_ "fedpkg" ["switch-branch", branch]
        cmd_ "git" ["pull", "-q"]
      -- noupdate <- doesFileExist ".noupdate"
      -- unless noupdate $
      --   void $ cmdBool "cabal-rpm" ["update"]
      let spec = pkg ++ ".spec"
      hasSpec <- doesFileExist spec
      if not hasSpec
        then putStrLn $ "No" +-+ spec
        else do
        nvr <- cmd "fedpkg" ["verrel"]
        let verrel = removePrefix (pkg ++ "-") nvr
            release = tail $ dropWhile (/= '-') verrel
            tag = distTag dist
            relver = releaseVersion dist
        case mode of
          Install -> do
            let req = fromMaybe pkg msubpkg
            installed <- cmdMaybe "rpm" ["-q", "--qf", "%{name}-%{version}-%{release}", req]
            if Just (req ++ "-" ++ verrel) == installed
              then putStrLn $ nvr +-+ "already installed!\n"
              else do
              putStrLn $ fromMaybe "Not installed" installed +-+ "->" +-+ nvr
              cmd_ "git" ["--no-pager", "log", "-1"]
              putStrLn ""
              missing <- catMaybes <$> nub <$> (buildRequires spec >>= filterM notInstalled >>= mapM (derefSrcPkg relver))
              -- FIXME sort into build order
              let hmissing = filter (\ dp -> "ghc-" `isPrefixOf` dp || dp `elem` ["alex", "cabal-install", "gtk2hs-buildtools", "happy"]) missing
              unless (null hmissing) $ do
                putStrLn "Missing:"
                mapM_ putStrLn hmissing
                build topdir Install dist Nothing Nothing False hmissing
                setCurrentDirectory $ topdir </> wd
              stillMissing <- filterM notInstalled missing
              unless (null stillMissing) $ do
                putStrLn $ "Installing:" +-+ intercalate ", " stillMissing
                rpmInstall stillMissing
              putStrLn ""
              putStrLn $ "Building" +-+ nvr
              -- note "fedpkg --path dir local" saves .build.log in cwd
              cmd_ "fedpkg" ["local"]
              opkgs <- lines <$> rpmspec ["--builtrpms"] (Just "%{name}\n") spec
              rpms <- lines <$> rpmspec ["--builtrpms"] (Just ("%{arch}/%{name}-%{version}-" ++ release ++ ".%{arch}.rpm\n")) spec
              putStrLn $ nvr +-+ "built\n"
              instpkgs <- lines <$> cmd "rpm" ("-qa":opkgs)
              if null instpkgs
                -- maybe filter out pandoc-pdf if not installed
                then rpmInstall rpms
                else do
                pkgmgr <- packageManager
                -- sudo pkgmgr ("--setopt=clean_requirements_on_remove=no":"remove":"-y":instpkgs)
                sudo pkgmgr ("install":"-y":rpms)
              setCurrentDirectory topdir
            build topdir Install dist Nothing Nothing False rest
          Mock -> do
            putStrLn $ "Mock building" +-+ nvr
            cmdlog "fedpkg" ["mockbuild"]
            build topdir Mock dist Nothing Nothing False rest
          Koji -> do
            putStrLn "'koji' mode is deprecated, please use 'chain'"
            unless (null rest) $ do
              putStrLn $ show (length rest) +-+ "packages left"
              putStrLn ""
            latest <- kojiLatestPkg tag pkg
            if nvr == latest
              then do
              putStrLn $ nvr +-+ "already built!"
              kojiWaitPkg tag nvr
              print mlast
              build topdir Koji dist Nothing mlast False rest
              else do
              building <- kojiBuilding pkg nvr
              if building
                then do
                putStrLn $ nvr +-+ "is already building"
                kojiWaitPkg tag nvr
                build topdir Koji dist Nothing Nothing False rest
                else do
                print mlast
                case mlast of
                  Nothing -> return ()
                  Just (pkg', nvr') -> do
                    dep <- dependent pkg' pkg branch topdir
                    when dep $ do
                      putStrLn $ "Waiting for" +-+ nvr'
                      kojiWaitPkg tag nvr'
                showChange latest nvr
                cmd_ "git" ["push"]
                fedpkgBuild dist nvr (if waitrepo then Just tag else Nothing)
                bodhiOverride dist nvr
                unless (null rest) $ do
                  dep <- dependent pkg (head rest) branch topdir
                  when dep $ do
                    putStrLn $ "Waiting for" +-+ nvr
                    kojiWaitPkg tag nvr
                  build topdir Koji dist Nothing (if dep then Just (pkg, nvr) else Nothing) False rest
          Pending -> do
            latest <- kojiLatestPkg tag pkg
            unless (eqNVR nvr latest) $
              putStrLn $ latest +-+ "->" +-+ nvr
            build topdir Pending dist Nothing Nothing False rest
          Changed -> do
            latest <- kojiLatestPkg tag pkg
            unless (eqNVR nvr latest) $
              putStrLn pkg
            build topdir Changed dist Nothing Nothing False rest
          Built -> do
            latest <- kojiLatestPkg tag pkg
            when (eqNVR nvr latest) $
              putStrLn pkg
            build topdir Built dist Nothing Nothing False rest
          Chain -> do
            latest <- kojiLatestPkg tag pkg
            if nvr == latest
              then do
              putStrLn $ nvr +-+ "already built!"
              unless (null rest) $ --do
                --kojiWaitPkg tag nvr
                build topdir Chain dist Nothing Nothing False rest
              else do
              building <- kojiBuilding pkg nvr
              if building
                then do
                putStrLn $ nvr +-+ "is already building"
                unless (null rest) $ --do
                  --kojiWaitPkg tag nvr
                  build topdir Chain dist Nothing Nothing False rest
                else do
                showChange latest nvr
                brs <- buildRequires spec
                --print brs
                -- FIXME sort into build order
                let hdeps = filter (\ dp -> "ghc-" `isPrefixOf` dp || dp `elem` ["alex", "cabal-install", "gtk2hs-buildtools", "happy"]) (brs \\ ["ghc-rpm-macros", "ghc-rpm-macros-extra", "ghc-Cabal-devel"])
                --print hdeps
                srcs <- filter (`notElem` ["ghc"]) . catMaybes . nub <$> mapM (derefSrcPkg relver) hdeps
                --print srcs
                hmissing <- nub <$> filterM (notInKoji branch topdir tag) srcs
                putStrLn ""
                unless (null hmissing) $ do
                  putStrLn "Missing:"
                  mapM_ putStrLn hmissing
                  build topdir Chain dist Nothing Nothing True hmissing
                  setCurrentDirectory $ topdir </> wd
                  putStrLn ""
                -- note "fedpkg --path dir local" saves .build.log in cwd
                cmd_ "git" ["push"]
                putStrLn ""
                fedpkgBuild dist nvr  (if waitrepo then Just tag else Nothing)
                bodhiOverride dist nvr
                unless (null rest) $ do
                  putStrLn ""
                  putStrLn $ show (length rest) +-+ "packages left"
                  build topdir Chain dist Nothing (Just (pkg, nvr)) waitrepo rest

pkgDir :: String -> String -> FilePath -> IO FilePath
pkgDir dir branch top = do
  b <- doesDirectoryExist $ top </> dir </> branch
  return $ top </> dir </> if b then branch else ""

maybePkgVer :: String -> Maybe String -> String
maybePkgVer pkg mver = pkg ++ maybe "" ("-" ++) mver

notInstalled :: String -> IO Bool
notInstalled pkg =
  not <$> cmdBool "rpm" ["--quiet", "-q", pkg]

showChange :: String -> String -> IO ()
showChange latest nvr = do
  cmd_ "git" ["--no-pager", "log", "-1"]
  putStrLn ""
  putStrLn $ latest +-+ "->" +-+ nvr ++ "\n"

fedpkgBuild :: Dist -> String -> Maybe String -> IO ()
fedpkgBuild dist nvr waittag = do
  cmd_ "fedpkg" $ "build" : maybe [] (\ d -> "--target":[d]) (distTarget dist)
  logMsg $ nvr +-+ "built"
  maybe (return ()) (`kojiWaitPkg` nvr) waittag

bodhiOverride :: Dist -> String -> IO ()
bodhiOverride dist nvr =
  when (distOverride dist) $
    -- FIXME: improve Notes with recursive info
    cmd_ "bodhi" ["overrides", "save", "--notes", "Haskell stack", nvr]

-- -- dereference meta BRs
-- whatProvides :: String -> String -> IO String
-- whatProvides relver pkg = do
--   res <- repoquery relver ["--qf", "%{name}", "--whatprovides", pkg]
--   --print res
--   when (null res) $ do
--     installed <- not <$> notInstalled pkg
--     unless installed $ putStrLn $ "Warning:" +-+ pkg +-+ "not found by repoquery"
--   return $ if null res then pkg else res

buildRequires :: FilePath -> IO [String]
buildRequires spec = do
  putStrLn "Repoquerying buildrequires..."
  (map (head . words) . lines) <$> rpmspec ["--buildrequires"] Nothing spec
--    >>= mapM (whatProvides relver)

derefSrcPkg :: String -> String -> IO (Maybe String)
derefSrcPkg relver pkg = do
  res <- repoquerySrc relver pkg
  --putStrLn $ pkg +-+ "->" +-+ show res
  case res of
    -- maybe package has never been built yet
    Nothing -> do
      let base = removeSuffix "-devel" pkg
      p <- pkgdb base
      if isJust p
        then return p
        else pkgdb $ removePrefix "ghc-" base
    Just s -> return $ Just s

gitBranch :: IO String
gitBranch =
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["branch"]

pkgdb :: String -> IO (Maybe String)
pkgdb pkg = do
  res <- fmap words <$> cmdMaybe "pkgdb-cli" ["list", "--nameonly", pkg]
  return $ case res of
    (Just ps) | pkg `elem` ps -> Just pkg
    _ -> Nothing

eqNVR :: String -> String -> Bool
eqNVR p1 p2 =
  dropExtension p1 == dropExtension p2

-- "pkg = X.Y" -> ["pkg", "=", "X.Y"] -> ("pkg", Just "X.Y")
processDeps :: [String] -> (String, Maybe String)
processDeps [p, "=", v] = (p, Just v)
processDeps (p:_) = (p, Nothing)
processDeps [] = error "processDeps: empty string!"

dependent :: String -> String -> String -> FilePath -> IO Bool
dependent dep pkg branch topdir = do
  pkgpath <- pkgDir pkg branch topdir
  cmdBool "grep" ["-q", dep, pkgpath </> pkg ++ ".spec"]

displayLogTail :: FilePath -> IO ()
displayLogTail f = do
  ls <- lines <$> readFile f
  let foot = 3
      disp = 12
      start = length ls - (disp + foot)
  mapM_ putStrLn $ take disp $ drop start ls
