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

import FedoraDists (Dist, distBranch, distOverride, rpmDistTag)
import Koji (kojiBuilding, kojiCheckFHBuilt, kojiLatestPkg, kojiWaitPkg,
             notInKoji, rpkg, rpkgBuild)
import RPM (buildRequires, derefSrcPkg, haskellSrcPkgs, Package,
            packageManager, pkgDir, rpmInstall, rpmspec)
import SimpleCmd ((+-+), cmd, cmd_, cmdBool, cmdLines, cmdMaybe, cmdlog,
                  cmdSilent, grep_, removeStrictPrefix, sudo)
import SimpleCmd.Git (git_, gitBranch, isGitDir)
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
readBuildCmd "notinstalled" = NotInstalled
readBuildCmd _ = error "Unknown command"

-- commands :: [String]
-- commands = ["install", "mock" , "koji", "chain", "pending", "changed", "built", 
--             "bump", "notinstalled"]

build :: FilePath -> Maybe String -> Maybe (String, String) -> Bool ->
         Command -> Dist -> [String] -> IO ()
build _ _ _ _ _ _ [] = return ()
build topdir msubpkg mlast waitrepo mode dist (pkg:rest) = do
  setCurrentDirectory topdir
  let branch = distBranch dist
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
      build topdir Nothing Nothing False mode dist rest
      else do
      pkggit <- do
        gd <- isGitDir "."
        if gd then checkPkgsGit
          else return False
      if not pkggit
        then if mode `elem` [Install, Koji, Chain]
             then error $ "not a Fedora pkg git dir!:" +-+ wd
             else build topdir Nothing Nothing False mode dist rest
        else do
        when dirExists $ do
          git_ "pull" ["-q"]
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
              let req = fromMaybe pkg msubpkg
              installed <- cmdMaybe "rpm" ["-q", "--qf", "%{name}-%{version}-%{release}", req]
              if Just (req ++ "-" ++ verrel) == installed
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
                  build topdir Nothing Nothing False Install dist srcs
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
                  build topdir Nothing Nothing False Install dist [pkg]
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
                    sudo pkgmgr ("--setopt=clean_requirements_on_remove=no":"remove":"-y":instpkgs)
                    sudo pkgmgr ("install":"-y":rpms)
              putStrLn ""
              putStrLn $ show (length rest) +-+ "packages left"
              build topdir Nothing Nothing False Install dist rest
            Mock -> do
              putStrLn $ "Mock building" +-+ nvr
              cmdlog (rpkg dist) ["mockbuild"]
              build topdir Nothing Nothing False Mock dist rest
            Koji -> do
              unless (null rest) $ do
                putStrLn $ show (length rest) +-+ "more packages"
                putStrLn ""
              latest <- kojiLatestPkg dist pkg
              if Just nvr == latest
                then do
                putStrLn $ nvr +-+ "already built!"
                kojiWaitPkg topdir dist nvr
                build topdir Nothing mlast False Koji dist rest
                else do
                building <- kojiBuilding pkg nvr dist
                if building
                  then do
                  putStrLn $ nvr +-+ "is already building"
                  kojiWaitPkg topdir dist nvr
                  build topdir Nothing Nothing False Koji dist rest
                  else do
                  case mlast of
                    Nothing -> return ()
                    Just (pkg', nvr') -> do
                      dep <- dependent pkg' pkg branch topdir
                      when dep $ do
                        putStrLn $ "Waiting for" +-+ nvr'
                        kojiWaitPkg topdir dist nvr'
                  showChange pkg latest nvr
                  putStrLn ""
                  git_ "push" []
                  rpkgBuild topdir dist nvr waitrepo
                  bodhiOverride dist nvr
                  unless (null rest) $ do
                    dep <- dependent pkg (head rest) branch topdir
                    when dep $ do
                      putStrLn $ "Waiting for" +-+ nvr
                      kojiWaitPkg topdir dist nvr
                    build topdir Nothing (if dep then Just (pkg, nvr) else Nothing) False Koji dist rest
            Pending -> do
              latest <- kojiLatestPkg dist pkg
              unless (eqNVR nvr latest) $
                showNVRChange pkg latest nvr
              build topdir Nothing Nothing False Pending dist rest
            Changed -> do
              latest <- kojiLatestPkg dist pkg
              unless (eqNVR nvr latest) $
                putStrLn pkg
              build topdir Nothing Nothing False Changed dist rest
            Built -> do
              latest <- kojiLatestPkg dist pkg
              when (eqNVR nvr latest) $
                putStrLn pkg
              build topdir Nothing Nothing False Built dist rest
            Bump -> do
              latest <- kojiLatestPkg dist pkg
              when (eqNVR nvr latest) $ do
                putStrLn pkg
                cmd_ "rpmdev-bumpspec" ["-c", "rebuild", spec]
                cmd_ (rpkg dist) ["commit", "-m", "bump release"]
              build topdir Nothing Nothing False Bump dist rest
            NotInstalled -> do
              opkg <- head <$> rpmspec ["--builtrpms"] (Just "%{name}") spec
              inst <- cmdMaybe "rpm" ["-q", opkg] :: IO (Maybe String)
              when (isNothing inst) $ putStrLn pkg
              build topdir Nothing Nothing False NotInstalled dist rest
            Chain -> do
              fhbuilt <- kojiCheckFHBuilt topdir nvr
              latest <- if fhbuilt
                         then return $ Just nvr
                         else kojiLatestPkg dist pkg
              if Just nvr == latest
                then do
                putStrLn $ nvr +-+ "already built!"
                unless (null rest) $ --do
                  --kojiWaitPkg topdir dist nvr
                  build topdir Nothing Nothing False Chain dist rest
                else do
                building <- kojiBuilding pkg nvr dist
                if building
                  then do
                  putStrLn $ nvr +-+ "is already building"
                  kojiWaitPkg topdir dist nvr
                  unless (null rest) $
                    build topdir Nothing Nothing False Chain dist rest
                  else do
                  showChange pkg latest nvr
                  putStrLn ""
                  srcs <- buildRequires spec >>= haskellSrcPkgs topdir dist
                  --print srcs
                  hmissing <- nub <$> filterM (notInKoji branch topdir dist) srcs
                  putStrLn ""
                  unless (null hmissing) $ do
                    putStrLn "Missing:"
                    mapM_ putStrLn hmissing
                    build topdir Nothing Nothing True Chain dist hmissing
                    setCurrentDirectory $ topdir </> wd
                    putStrLn ""
                  -- note "fedpkg --path dir local" saves .build.log in cwd
                  git_ "push" []
                  putStrLn ""
                  rpkgBuild topdir dist nvr waitrepo
                  bodhiOverride dist nvr
                  unless (null rest) $ do
                    putStrLn ""
                    putStrLn $ show (length rest) +-+ "packages left"
                    build topdir Nothing (Just (pkg, nvr)) waitrepo Chain dist rest

notInstalled :: String -> IO Bool
notInstalled pkg =
  not <$> cmdBool "rpm" ["--quiet", "-q", pkg]

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

bodhiOverride :: Dist -> String -> IO ()
bodhiOverride dist nvr =
  when (distOverride dist) $
    -- FIXME: improve Notes with recursive info
    cmd_ "bodhi" ["overrides", "save", "--notes", "Haskell stack", nvr]

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
