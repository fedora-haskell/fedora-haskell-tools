{-# LANGUAGE CPP #-}

-- |
-- Module      :  Build
-- Copyright   :  (C) 2014-2019  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: Main entry point for building RPM packages.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Build
  (build,
   readBuildCmd
  )
where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, unless, void, when)
import Data.Maybe
import Data.List (intercalate, isPrefixOf, nub)

import System.Directory (doesDirectoryExist, doesFileExist,
                         setCurrentDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), dropExtension)

import Distribution.Fedora (Dist, distBranch, getLatestFedoraDist, rpmDistTag)
import Dist (distOverride, distRemote)
import Koji (kojiBuilding, kojiCheckFHBuilt, kojiLatestPkg, kojiListTags,
              kojiWaitPkg, notInKoji, rpkg, rpkgBuild)
import RPM (buildRequires, derefSrcPkg, haskellSrcPkgs, Package,
            packageManager, pkgDir, rpmInstall, rpmspec)
import SimpleCmd ((+-+), cmd, cmd_, cmdBool, cmdLines, cmdLog, cmdMaybe,
                  cmdSilent, grep_, removeStrictPrefix, sudo_)
import SimpleCmd.Git (git_, gitBool, gitBranch, isGitDir)
import Utils (checkPkgsGit)

data Command = Install | Mock | Koji | Chain | Pending | Changed | Built | Bump
             | NotInstalled
             deriving (Eq)

readBuildCmd :: String -> Command
readBuildCmd "install" = Install
readBuildCmd "mock" = Mock
readBuildCmd "koji" = Koji
readBuildCmd "chain" = Chain
readBuildCmd "pending" = Pending
readBuildCmd "changed" = Changed
readBuildCmd "built" = Built
readBuildCmd "bump" = Bump
readBuildCmd "not-installed" = NotInstalled
readBuildCmd _ = error "Unknown command"

build :: FilePath -> Maybe (String, String) -> Bool ->
         Command -> Dist -> [String] -> IO ()
build _ _ _ _ _ [] = return ()
build topdir mlast waitrepo mode dist (pkg:rest) = do
  setCurrentDirectory topdir
  branched <- getLatestFedoraDist
  let branch = distBranch branched dist
  when (mode `notElem` [Pending, Changed, Built, NotInstalled]) $
    putStrLn $ "\n==" +-+ pkg ++ ":" ++ branch +-+ "=="
  dirExists <- doesDirectoryExist pkg
  unless dirExists $ do
    let anon = ["-a" | mode `notElem` [Koji, Chain]]
    cmdSilent (rpkg dist) $ ["clone", "-b", branch, pkg] ++ anon
  wd <- pkgDir pkg branch ""
  setCurrentDirectory wd
  ignore <- doesFileExist ".fhbuild-ignore"
  if ignore && mode `elem` [Install, Koji, Chain, Mock] then putStrLn "ignored"
    else do
    failing <- doesFileExist ".fhbuild-fail"
    when (failing && mode /= Pending) $ do
      putStrLn "skipped: found '.fhbuild-fail' file"
      exitWith (ExitFailure 1)
    retired <- doesFileExist "dead.package"
    if retired then do
      unless (mode `elem` [NotInstalled,Pending]) $
        putStrLn "skipping dead.package"
      build topdir Nothing False mode dist rest
      else do
      pkggit <- do
        gd <- isGitDir "."
        if gd then checkPkgsGit
          else return False
      if not pkggit
        then if mode `elem` [Install, Koji, Chain]
             then error $ "not a Fedora pkg git dir!:" +-+ wd
             else build topdir Nothing False mode dist rest
        else do
        when dirExists $ do
          pulled <- gitBool "pull" ["-q", "--rebase"]
          unless pulled $ error $ "git pull failed for " ++ pkg
          actual <- gitBranch
          when (branch /= actual) $
            cmd_ (rpkg dist) ["switch-branch", branch]
        -- noupdate <- doesFileExist ".noupdate"
        -- unless noupdate $
        --   void $ cmdBool "cabal-rpm" ["update"]
        let spec = pkg ++ ".spec"
        hasSpec <- doesFileExist spec
        if not hasSpec
          then error $ "No" +-+ spec
          else do
          nvr <- cmd (rpkg dist) ["verrel"]
          let verrel = removeStrictPrefix (pkg ++ "-") nvr
          case mode of
            Install -> do
              installed <- cmdMaybe "rpm" ["-q", "--qf", "%{name}-%{version}-%{release}", pkg]
              if Just (pkg ++ "-" ++ verrel) == installed
                then putStrLn $ nvr +-+ "already installed!\n"
                else do
                putStrLn $ fromMaybe "Not installed" installed +-+ "->" +-+ nvr
                git_ "log" ["-1"]
                putStrLn ""
                missing <- nub <$> (buildRequires spec >>= filterM notInstalled)
                -- FIXME sort into build order
                let hmissing = filter (\ dp -> "ghc-" `isPrefixOf` dp || dp `elem` ["alex", "cabal-install", "gtk2hs-buildtools", "happy"]) missing
                srcs <- nub <$> mapM (derefSrcPkg topdir dist True) hmissing
                unless (null srcs) $ do
                  putStrLn "Missing:"
                  mapM_ putStrLn srcs
                  build topdir Nothing False Install dist srcs
                  setCurrentDirectory $ topdir </> wd
                stillMissing <- filterM notInstalled missing
                unless (null stillMissing) $ do
                  putStrLn $ "Installing:" +-+ intercalate ", " stillMissing
                  rpmInstall stillMissing
                putStrLn ""
                putStrLn $ "Building" +-+ nvr
                -- note "fedpkg --path dir local" saves .build.log in cwd
                success <- cmdBool (rpkg dist) ["local"]
                if not success
                  then do
                  waitForEnter
                  build topdir Nothing False Install dist [pkg]
                  else do
                  opkgs <- rpmspec ["--builtrpms"] (Just "%{name}") spec
                  rpms <- rpmspec ["--builtrpms", "--define=dist" +-+ rpmDistTag dist] (Just "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm") spec
                  putStrLn $ nvr +-+ "built\n"
                  instpkgs <- cmdLines "rpm" ("-qa":opkgs)
                  if null instpkgs
                    -- maybe filter out pandoc-pdf and xmonad* if not installed
                    then rpmInstall rpms
                    else do
                    pkgmgr <- packageManager
                    sudo_ pkgmgr ("--setopt=clean_requirements_on_remove=no":"remove":"-y":instpkgs)
                    sudo_ pkgmgr ("install":"-y":rpms)
              putStrLn ""
              putStrLn $ show (length rest) +-+ "packages left"
              build topdir Nothing False Install dist rest
            Mock -> do
              putStrLn $ "Mock building" +-+ nvr
              cmdLog (rpkg dist) ["mockbuild"]
              build topdir Nothing False Mock dist rest
            Koji -> do
              unless (null rest) $ do
                putStrLn $ show (length rest) +-+ "more packages"
                putStrLn ""
              latest <- kojiLatestPkg dist pkg
              if eqNVR nvr latest
                then do
                putStrLn $ fromJust latest +-+ "already built!"
                kojiWaitPkg topdir dist nvr
                build topdir mlast False Koji dist rest
                else do
                git_ "diff" []
                building <- kojiBuilding pkg nvr dist
                if building
                  then do
                  putStrLn $ nvr +-+ "is already building"
                  kojiWaitPkg topdir dist nvr
                  build topdir Nothing False Koji dist rest
                  else do
                  case mlast of
                    Nothing -> return ()
                    Just (pkg', nvr') -> do
                      dep <- dependent pkg' pkg branch topdir
                      when dep $
                        kojiWaitPkg topdir dist nvr'
                  showChange pkg latest nvr
                  putStrLn ""
                  git_ "push" []
                  -- note removed waitrepo:
                  rpkgBuild dist nvr
                  bodhiOverride branched dist nvr
                  unless (null rest) $ do
                    dep <- dependent pkg (head rest) branch topdir
                    when dep $
                      kojiWaitPkg topdir dist nvr
                    build topdir (if dep then Just (pkg, nvr) else Nothing) False Koji dist rest
            Pending -> do
              latest <- kojiLatestPkg dist pkg
              unless (eqNVR nvr latest) $
                showNVRChange pkg latest nvr
              build topdir Nothing False Pending dist rest
            Changed -> do
              latest <- kojiLatestPkg dist pkg
              unless (eqNVR nvr latest) $
                putStrLn pkg
              build topdir Nothing False Changed dist rest
            Built -> do
              latest <- kojiLatestPkg dist pkg
              when (eqNVR nvr latest) $
                putStrLn pkg
              build topdir Nothing False Built dist rest
            Bump -> do
              latest <- kojiLatestPkg dist pkg
              when (eqNVR nvr latest) $ do
                git_ "log" [distRemote branched dist ++ "..HEAD", "--pretty=oneline"]
                cmd_ "rpmdev-bumpspec" ["-c", "refresh to cabal-rpm-2.0.2", spec]
--                cmd_ (rpkg dist) ["commit", "-m", "cabal-rpm-1.0.0: add doc and prof subpkgs"]
                git_ "commit" ["-a", "--amend", "--no-edit"{-, "-m", "revised .cabal"-}]
              build topdir Nothing False Bump dist rest
            NotInstalled -> do
              opkg <- head <$> rpmspec ["--builtrpms"] (Just "%{name}") spec
              inst <- cmdMaybe "rpm" ["-q", opkg] :: IO (Maybe String)
              when (isNothing inst) $ putStrLn pkg
              build topdir Nothing False NotInstalled dist rest
            Chain -> do
              fhbuilt <- kojiCheckFHBuilt topdir nvr
              latest <- if fhbuilt
                         then return $ Just nvr
                         else kojiLatestPkg dist pkg
              if eqNVR nvr latest
                then do
                if fhbuilt
                  then putStrLn $ fromJust latest +-+ "already built!"
                  else kojiWaitPkg topdir dist nvr
                unless (null rest) $
                  build topdir Nothing False Chain dist rest
                else do
                tags <- kojiListTags dist nvr
                if any ((show dist ++ "-updates-") `isPrefixOf`) tags
                  then do
                  putStrLn $ nvr +-+ "tags:" +-+ unwords tags
                  bodhiOverride branched dist nvr
                  build topdir Nothing True Chain dist rest
                  else do
                  git_ "diff" []
                  building <- kojiBuilding pkg nvr dist
                  if building
                    then do
                    putStrLn $ nvr +-+ "is already building"
                    kojiWaitPkg topdir dist nvr
                    unless (null rest) $
                      build topdir Nothing False Chain dist rest
                    else do
                    showChange pkg latest nvr
                    putStrLn ""
                    srcs <- buildRequires spec >>= haskellSrcPkgs topdir dist
                    hmissing <- nub <$> filterM (notInKoji branch topdir dist) srcs
                    putStrLn ""
                    unless (null hmissing) $ do
                      putStrLn "Deps:"
                      mapM_ putStrLn hmissing
                      build topdir Nothing True Chain dist hmissing
                      setCurrentDirectory $ topdir </> wd
                      putStrLn ""
                    git_ "push" []
                    putStrLn ""
                    rpkgBuild dist nvr
                    -- may need waitrepo here?
                    bodhiOverride branched dist nvr
                    unless (null rest) $ do
                      putStrLn ""
                      putStrLn $ show (length rest) +-+ "packages left"
                      build topdir (Just (pkg, nvr)) waitrepo Chain dist rest

notInstalled :: String -> IO Bool
notInstalled pkg =
  not <$> cmdBool "rpm" ["--quiet", "-q", "--whatprovides", pkg]

showChange :: Package -> Maybe String -> String -> IO ()
showChange pkg mlatest nvr = do
  git_ "log" ["-1"]
  putStrLn ""
  showNVRChange pkg mlatest nvr

showNVRChange :: Package -> Maybe String -> String -> IO ()
showNVRChange pkg Nothing nvr =
  putStrLn $ pkg +-+ "new" +-+ removeStrictPrefix (pkg ++ "-") nvr
showNVRChange pkg (Just latest) nvr = do
  putStrLn $ pkg ++ ":" +-+ removeStrictPrefix prefix latest
  putStrLn $ replicate (length pkg - 1) ' ' ++ "->" +-+ removeStrictPrefix prefix nvr
  where
    prefix = pkg ++ "-"

bodhiOverride :: Dist -> Dist -> String -> IO ()
bodhiOverride branched dist nvr = do
  when (distOverride branched dist) $
    -- FIXME: improve Notes with recursive info
    cmdLog "bodhi" ["overrides", "save", "--notes", "Haskell stack", nvr]

eqNVR :: String -> Maybe String -> Bool
eqNVR p1 p2 =
  Just (dropExtension p1) == fmap dropExtension p2

dependent :: String -> String -> String -> FilePath -> IO Bool
dependent dep pkg branch topdir = do
  pkgpath <- pkgDir pkg branch topdir
  grep_ dep $ pkgpath </> pkg ++ ".spec"

waitForEnter :: IO ()
waitForEnter = do
    putStrLn ""
    putChar '\a'
    putStrLn "Press Enter after fixing"
    void getLine
