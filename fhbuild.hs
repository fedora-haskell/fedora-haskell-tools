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
import Control.Monad (filterM, unless, void, when)
import Data.Maybe
import Data.List (intercalate, isPrefixOf, nub)

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), dropExtension)
import System.IO (hPutStrLn, stderr)

import Dists (Dist, dists, distBranch, distOverride, distTag, distTarget,
              rpmDistTag)
import Koji (koji, kojiBuilding, kojiCheckFHBuilt, kojiLatestPkg, kojiWaitPkg,
             notInKoji, rpkg)
import RPM (buildRequires, derefSrcPkg, haskellSrcPkgs, Package,
            packageManager, pkgDir, rpmInstall, rpmspec)
import Utils ((+-+), checkPkgsGit, cmd, cmd_, cmdBool, cmdMaybe, cmdlog,
              git_, gitBranch, logMsg, removePrefix, removeSuffix, sudo)

data Command = Install | Mock | Koji | Chain | Pending | Changed | Built | Bump
             deriving (Eq)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (c:d:ps) -> do
      (com, dist, pkgs) <- parseArgs c d ps
      cwd <- getCurrentDirectory
      build cwd (mode com) dist Nothing Nothing False pkgs
    _ -> help
  where
    mode "install" = Install
    mode "mock" = Mock
    mode "koji" = Koji
    mode "chain" = Chain
    mode "pending" = Pending
    mode "changed" = Changed
    mode "built" = Built
    mode "bump" = Bump
    mode _ = error "Unknown command"

commands :: [String]
commands = ["install", "mock" , "koji", "chain", "pending", "changed", "built", 
            "bump"]

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
    ++ "  bump\t\t- bump release for NVRs already built\n"
  exitWith (ExitFailure 1)

type Arguments = (String, Dist, [String])

parseArgs :: String -> String -> [String] -> IO Arguments
parseArgs c dist pkgs | dist `notElem` dists && not ("rhel-" `isPrefixOf` dist) = error $ "Unknown dist '" ++ dist ++ "'"
                      | null pkgs = error "Please specify a package."
                      | otherwise =
                          return (c, dist, map (removeSuffix "/") pkgs)

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
    cmdlog (rpkg (Just dist)) $ ["clone", "-b", branch, pkg] ++ anon
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
      unless (mode == Pending) $
        putStrLn "skipping dead.package"
      build topdir mode dist Nothing Nothing False rest
      else do
      pkggit <- do
        gd <- doesFileExist ".git/config"
        if gd then checkPkgsGit
          else return False
      if not pkggit
        then if mode `elem` [Install, Koji, Chain]
             then error $ "not a Fedora pkg git dir!:" +-+ wd
             else build topdir mode dist Nothing Nothing False rest
        else do
        when dirExists $ do
          actual <- gitBranch
          when (branch /= actual) $
            cmd_ (rpkg (Just dist)) ["switch-branch", branch]
          git_ "pull" ["-q"]
        -- noupdate <- doesFileExist ".noupdate"
        -- unless noupdate $
        --   void $ cmdBool "cabal-rpm" ["update"]
        let spec = pkg ++ ".spec"
        hasSpec <- doesFileExist spec
        if not hasSpec
          then putStrLn $ "No" +-+ spec
          else do
          nvr <- cmd (rpkg (Just dist)) ["verrel"]
          let verrel = removePrefix (pkg ++ "-") nvr
              tag = distTag dist
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
                  build topdir Install dist Nothing Nothing False srcs
                  setCurrentDirectory $ topdir </> wd
                stillMissing <- filterM notInstalled missing
                unless (null stillMissing) $ do
                  putStrLn $ "Installing:" +-+ intercalate ", " stillMissing
                  rpmInstall stillMissing
                putStrLn ""
                putStrLn $ "Building" +-+ nvr
                -- note "fedpkg --path dir local" saves .build.log in cwd
                success <- cmdBool (rpkg (Just dist)) ["local"]
                if not success
                  then do
                  waitForEnter
                  build topdir Install dist Nothing Nothing False [pkg]
                  else do
                  opkgs <- lines <$> rpmspec ["--builtrpms"] (Just "%{name}\n") spec
                  rpms <- lines <$> rpmspec ["--builtrpms", "--define=dist" +-+ rpmDistTag dist] (Just "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm\n") spec
                  putStrLn $ nvr +-+ "built\n"
                  instpkgs <- lines <$> cmd "rpm" ("-qa":opkgs)
                  if null instpkgs
                    -- maybe filter out pandoc-pdf if not installed
                    then rpmInstall rpms
                    else do
                    pkgmgr <- packageManager
                    sudo pkgmgr ("--setopt=clean_requirements_on_remove=no":"remove":"-y":instpkgs)
                    sudo pkgmgr ("install":"-y":rpms)
              putStrLn ""
              putStrLn $ show (length rest) +-+ "packages left"
              build topdir Install dist Nothing Nothing False rest
            Mock -> do
              putStrLn $ "Mock building" +-+ nvr
              cmdlog (rpkg (Just dist)) ["mockbuild"]
              build topdir Mock dist Nothing Nothing False rest
            Koji -> do
              unless (null rest) $ do
                putStrLn $ show (length rest) +-+ "more packages"
                putStrLn ""
              latest <- kojiLatestPkg tag pkg
              if Just nvr == latest
                then do
                putStrLn $ nvr +-+ "already built!"
                kojiWaitPkg topdir tag nvr
                build topdir Koji dist Nothing mlast False rest
                else do
                building <- kojiBuilding pkg nvr dist
                if building
                  then do
                  putStrLn $ nvr +-+ "is already building"
                  kojiWaitPkg topdir tag nvr
                  build topdir Koji dist Nothing Nothing False rest
                  else do
                  case mlast of
                    Nothing -> return ()
                    Just (pkg', nvr') -> do
                      dep <- dependent pkg' pkg branch topdir
                      when dep $ do
                        putStrLn $ "Waiting for" +-+ nvr'
                        kojiWaitPkg topdir tag nvr'
                  showChange pkg latest nvr
                  putStrLn ""
                  git_ "push" []
                  rpkgBuild topdir dist nvr (if waitrepo then Just tag else Nothing)
                  bodhiOverride dist nvr
                  unless (null rest) $ do
                    dep <- dependent pkg (head rest) branch topdir
                    when dep $ do
                      putStrLn $ "Waiting for" +-+ nvr
                      kojiWaitPkg topdir tag nvr
                    build topdir Koji dist Nothing (if dep then Just (pkg, nvr) else Nothing) False rest
            Pending -> do
              latest <- kojiLatestPkg tag pkg
              unless (eqNVR nvr latest) $
                showNVRChange pkg latest nvr
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
            Bump -> do
              latest <- kojiLatestPkg tag pkg
              when (eqNVR nvr latest) $ do
                putStrLn pkg
                cmd_ "rpmdev-bumpspec" ["-c", "rebuild", spec]
                cmd_ (rpkg (Just dist)) ["commit", "-m", "bump release"]
              build topdir Bump dist Nothing Nothing False rest
            Chain -> do
              fhbuilt <- kojiCheckFHBuilt topdir nvr
              latest <- if fhbuilt
                         then return $ Just nvr
                         else kojiLatestPkg tag pkg
              if Just nvr == latest
                then do
                putStrLn $ nvr +-+ "already built!"
                unless (null rest) $ --do
                  --kojiWaitPkg topdir tag nvr
                  build topdir Chain dist Nothing Nothing False rest
                else do
                building <- kojiBuilding pkg nvr dist
                if building
                  then do
                  putStrLn $ nvr +-+ "is already building"
                  kojiWaitPkg topdir tag nvr
                  unless (null rest) $
                    build topdir Chain dist Nothing Nothing False rest
                  else do
                  showChange pkg latest nvr
                  putStrLn ""
                  srcs <- buildRequires spec >>= haskellSrcPkgs topdir dist
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
                  git_ "push" []
                  putStrLn ""
                  rpkgBuild topdir dist nvr  (if waitrepo then Just tag else Nothing)
                  bodhiOverride dist nvr
                  unless (null rest) $ do
                    putStrLn ""
                    putStrLn $ show (length rest) +-+ "packages left"
                    build topdir Chain dist Nothing (Just (pkg, nvr)) waitrepo rest

maybePkgVer :: String -> Maybe String -> String
maybePkgVer pkg mver = pkg ++ maybe "" ("-" ++) mver

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
  putStrLn $ pkg +-+ "new" +-+ removePrefix (pkg ++ "-") nvr
showNVRChange pkg (Just latest) nvr = do
  putStrLn $ pkg ++ ":" +-+ removePrefix prefix latest
  putStrLn $ replicate (length pkg - 1) ' ' ++ "->" +-+ removePrefix prefix nvr
  where
    prefix = pkg ++ "-"

rpkgBuild :: FilePath -> Dist -> String -> Maybe String -> IO ()
rpkgBuild topdir dist nvr waittag = do
  giturl <- cmd (rpkg (Just dist)) ["giturl"]
  success <- cmdBool (koji dist) ["build", "--fail-fast", distTarget dist, giturl]
  if success
    then do
    logMsg $ nvr +-+ "built"
    maybe (return ()) (\ t -> kojiWaitPkg topdir t nvr) waittag
    else waitForEnter

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

eqNVR :: String -> Maybe String -> Bool
eqNVR p1 p2 =
  Just (dropExtension p1) == fmap dropExtension p2

-- "pkg = X.Y" -> ["pkg", "=", "X.Y"] -> ("pkg", Just "X.Y")
processDeps :: [String] -> (String, Maybe String)
processDeps [p, "=", v] = (p, Just v)
processDeps (p:_) = (p, Nothing)
processDeps [] = error "processDeps: empty string!"

dependent :: String -> String -> String -> FilePath -> IO Bool
dependent dep pkg branch topdir = do
  pkgpath <- pkgDir pkg branch topdir
  grep dep $ pkgpath </> pkg ++ ".spec"

displayLogTail :: FilePath -> IO ()
displayLogTail f = do
  ls <- lines <$> readFile f
  let foot = 3
      disp = 12
      start = length ls - (disp + foot)
  mapM_ putStrLn $ take disp $ drop start ls

waitForEnter :: IO ()
waitForEnter = do
    putStrLn ""
    putChar '\a'
    putStrLn "Press Enter after fixing"
    void getLine
