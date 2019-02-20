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

module Main where

import Control.Applicative (optional, some
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
                           ,(<$>)
#endif
                           )
import Control.Monad (filterM, unless, when, (>=>))
import Data.Maybe
import Data.List (isInfixOf, isPrefixOf, nub, sort, (\\))

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Options.Applicative (Parser, auto, option, switch, strOption)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getHomeDirectory,
#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
                         listDirectory,
#else
                         getDirectoryContents,
#endif
                         setCurrentDirectory)
import System.FilePath ((</>), (<.>), takeFileName)
import System.IO (BufferMode(..), hSetBuffering, stdout)
--import System.Posix.Env (getEnv)
import Text.CSV (parseCSV)

import FedoraDists (Dist(..), distBranch, distRepo, distUpdates,
                    hackageRelease, rawhide)

import SimpleCmd ((+-+), cmd, cmd_, cmdBool, cmdMaybe, cmdSilent, grep_,
              removePrefix)
import SimpleCmd.Git (git, git_, gitBranch, isGitDir)
import SimpleCmdArgs

import Koji (kojiListPkgs, rpkg)
import Options (distArg)
import Paths_fedora_haskell_tools (version)
import RPM (buildRequires, haskellSrcPkgs, Package, pkgDir,
            repoquery, rpmspec)
import Utils (checkPkgsGit, withCurrentDirectory)

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
#else
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- args <- getArgs
  simpleCmdArgs (Just version) "Fedora Haskell packages tool"
    "Fedora packages maintenance tool" $
    subcommands
    [ Subcommand "bump" "bump package release" $
      bump <$> strOption (optionMods 'm' "message" "CHANGELOG" "changelog message") <*> distArg <*> pkgArgs
    , Subcommand "checkout" "fedpkg switch-branch" $
      repoAction_ True False (return ()) <$> distArg <*> pkgArgs
    , Subcommand "clone"  "clone repos" $
      clone <$> branching <*> distArg <*> pkgArgs
    , Subcommand "clone-new" "clone new packages" $
      cloneNew <$> branching <*> distArg
    , Subcommand "cmd" "arbitrary command (with args)" $
      execCmd <$> strOption (optionMods 'c' "cmd" "CMD" "command to execute") <*> distArg <*> pkgArgs
    , Subcommand "count" "count number of packages" $
      (repoqueryHaskell False >=> (print . length)) <$> distArg
    , Subcommand "diff" "git diff" $
      gitDiff <$> gitDiffOpts <*> distArg <*> pkgArgs
    , Subcommand "diff-origin" "git diff origin" $
      gitDiffOrigin <$> distArg <*> pkgArgs
    , Subcommand "diff-branch" "compare branch with master" $
      repoAction False True compareRawhide <$> distArg <*> pkgArgs
    , Subcommand "diff-stackage" "compare with stackage" $
      diffStackage <$> switch (switchMods 'm' "missing" "only list missing packages") <*> distArg <*> pkgArgs
    , Subcommand "hackage" "generate Hackage distro data" $
      pure $ repoqueryHackageCSV hackageRelease
    , Subcommand "hackage-compare" "compare with Hackage distro data" $
      pure hackageCompare
    , Subcommand "head-origin" "head in sync with origin" $
      headOrigin  <$> distArg <*> pkgArgs
    , Subcommand "leaf" "list leaf packages" $
      leaves <$> switch (switchMods 'v' "deps" "show also deps") <*> distArg <*> pkgArgs
    , Subcommand "list" "list packages" $ mapM_ putStrLn <$> pkgArgs
    , Subcommand "merge" "git merge" $
      merge <$> strOption (optionMods 'f' "from" "BRANCH" "specify branch to merge from") <*> distArg <*> pkgArgs
    , Subcommand "missing" "missing dependency source packages" $
      missingDeps <$> distArg <*> pkgArgs
    , Subcommand "new" "new unbuilt packages" $
      (newPackages >=> mapM_ putStrLn) <$> distArg
    , Subcommand "old-packages" "packages not in repoquery" $
      oldPackages <$> distArg <*> pkgArgs
    , Subcommand "prep" "fedpkg prep" $
      prep <$> distArg <*> pkgArgs
    , Subcommand "commit" "fedpkg commit" $
      commit <$> strOption (optionMods 'm' "message" "COMMITMSG" "commit message") <*> distArg <*> pkgArgs
    , Subcommand "pull" "git pull repos" $
      repoAction_ True False (git_ "pull" ["--rebase"]) <$> distArg <*> pkgArgs
    , Subcommand "push" "git push repos" $
      repoAction_ True False (git_ "push" []) <$> distArg <*> pkgArgs
    , Subcommand "unpushed" "show unpushed commits" $
      unpushed <$> switch (switchMods 's' "short" "no log") <*> distArg <*> pkgArgs
    , Subcommand "update" "cabal-rpm update" $
      repoAction True True (updateOrRefreshPackage False) <$> distArg <*> pkgArgs
    , Subcommand "refresh" "cabal-rpm refresh" $
      repoAction True True (updateOrRefreshPackage True) <$> distArg <*> pkgArgs
    , Subcommand "subpkgs" "list subpackages" $
      repoAction True True (\ p -> rpmspec [] (Just "%{name}-%{version}") (p <.> "spec") >>= mapM_ putStrLn) <$> distArg <*> pkgArgs
    , Subcommand "verrel" "show nvr of packages" $
      verrel <$> distArg <*> pkgArgs
    ]
  where
    pkgArgs = some (strArg "PKG...")

    branching = switch (switchMods 'B' "branches" "clone branch dirs (fedpkg clone -B)")

    gitDiffOpts :: Parser GitDiffOpts
    gitDiffOpts = GitDiffOpts <$>
      optional (strOption (optionMods 'w' "with-branch" "BRANCH" "Branch to compare")) <*>
      switch (switchMods 's' "short" "Just output package name") <*>
      optional (option auto (optionMods 'u' "unified" "CONTEXT" "Lines of context"))

data GitDiffOpts = GitDiffOpts
  { withBranch :: Maybe String
  , short :: Bool
  , context :: Maybe Int }

bump :: String -> Dist -> [Package] -> IO ()
bump msg =
  repoAction True False bumpspec
  where
    bumpspec pkg =
      cmd_ "rpmdev-bumpspec" ["-c", msg, pkg <.> "spec"]

clone :: Bool -> Dist -> [Package] -> IO ()
clone True dist pkgs = cloneAllBranches dist pkgs
clone False dist pkgs =
  repoAction_ True False (return ()) dist pkgs

cloneNew :: Bool -> Dist -> IO ()
cloneNew True dist =
  newPackages rawhide >>= cloneAllBranches dist
cloneNew False dist =
  newPackages dist >>= repoAction_ True False (return ()) dist

execCmd :: String -> Dist -> [Package] -> IO ()
execCmd "" _ _ = error "CMD string must be given"
execCmd cs dist pkgs =
  repoAction_ True True (cmd_ c args) dist pkgs
  where
    (c:args) = words cs

gitDiff :: GitDiffOpts -> Dist -> [Package] -> IO ()
gitDiff opts =
  repoAction False False doGitDiff
  where
    doGitDiff pkg = do
      let mbrnch = withBranch opts
          branch = maybeToList mbrnch
          mcontxt = context opts
          contxt = maybe [] (\ n -> ["-U" ++ n]) $ show <$> mcontxt
      out <- git "diff" $ branch ++ contxt
      unless (null out) $ do
        putStrLn $ if short opts then pkg else "==" +-+ pkg +-+ "=="
        unless (short opts) $ putStrLn out

gitDiffOrigin :: Dist -> [Package] -> IO ()
gitDiffOrigin dist =
  repoAction_ True False (git_ "diff" ["origin/" ++ show dist]) dist

diffStackage :: Bool -> Dist -> [Package] -> IO ()
diffStackage missingOnly dist =
  repoAction False True compareStackage dist
  where
    compareStackage :: Package -> IO ()
    compareStackage p = do
      nvr <- cmd (rpkg dist) ["verrel"]
      let stream = "lts-12"
      stkg <- cmdMaybe "stackage" ["package", stream, removePrefix "ghc-" p]
      let same = isJust stkg && fromJust stkg `isInfixOf` nvr
      unless missingOnly $
        putStrLn $ nvr +-+ "(fedora)"
      if missingOnly then when (isNothing stkg) $ putStrLn p
        else
        putStrLn $ (if same then "same" else fromMaybe "none" stkg) +-+ "(" ++ stream ++ ")"

hackageCompare :: IO ()
hackageCompare =
  repoqueryHaskell False hackageRelease >>=
  compareHackage hackageRelease
  where
    compareHackage :: Dist -> [Package] -> IO ()
    compareHackage dist pkgs' = do
      hck <- simpleHTTP (getRequest "http://hackage.haskell.org/distro/Fedora/packages.csv") >>= getResponseBody
      let hackage = sort . either (error "Malformed Hackage csv") (map mungeHackage) $ parseCSV "packages.csv" hck
      fedora <- sort . map mungeRepo . lines <$> repoquery dist (["--repo=fedora", "--repo=updates", "--latest-limit=1", "--qf=%{name},%{version}"] ++ pkgs')
      compareSets True hackage fedora

    mungeHackage :: [String] -> PkgVer
    mungeHackage [_,v,u] = PV (takeFileName u) v
    mungeHackage _ = error "Malformed Hackage csv"

    mungeRepo :: String -> PkgVer
    mungeRepo s | ',' `elem` s =
                  let (p,v) = break (== ',') s in
                    PV p (tail v)
                | otherwise = error "Malformed repoquery output"

    compareSets :: Bool -> [PkgVer] -> [PkgVer] -> IO ()
    compareSets _ [] [] = return ()
    compareSets _ [] (f:fs) = do
      putStrLn ("New:" +-+ show f)
      compareSets False [] fs
    compareSets all' (h:hs) [] = do
      when all' $ putStrLn ("Removed:" +-+ show h)
      compareSets all' hs []
    compareSets all' (h:hs) (f:fs) | h == f = compareSets all' hs fs
                                   | h < f = do
                                       when all' $ putStrLn $ "Removed:" +-+ show h
                                       compareSets all' hs (f:fs)
                                   | h > f = do
                                       putStrLn $ "New:" +-+ show f
                                       compareSets all' (h:hs) fs
                                   | otherwise = do
                                       putStrLn $ pvPkg h ++ ":" +-+ pvVer h +-+ "->" +-+ pvVer f
                                       compareSets all' hs fs

headOrigin :: Dist -> [Package] -> IO ()
headOrigin dist =
  repoAction False False gitHeadAtOrigin dist
  where
    gitHeadAtOrigin :: Package -> IO ()
    gitHeadAtOrigin pkg = do
      -- use gitDiffQuiet
      same <- cmdBool "git" ["diff", "--quiet", "origin/" ++ show dist ++ "..HEAD"]
      when same $ putStrLn pkg

leaves :: Bool -> Dist -> [Package] -> IO ()
leaves verb =
  repoAction verb True checkLeafPkg
  where
    -- FIXME: make a dependency cache
    checkLeafPkg :: Package -> IO ()
    checkLeafPkg pkg = do
      dir <- takeFileName <$> getCurrentDirectory
      let branchdir = dir /= pkg
          top = if branchdir then "../.." else ".."
          spec = pkg <.> "spec"
      subpkgs <- rpmspec ["--builtrpms"] (Just "%{name}") spec
      allpkgs <- listDirectory top
      let other = map (\ p -> top </> p </> (if branchdir then dir else "") </> p <.> "spec") $ allpkgs \\ [pkg]
      found <- filterM (dependsOn subpkgs) other
      if null found
        then putStrLn pkg
        else when verb $ mapM_ putStrLn found
        where
          dependsOn :: [Package] -> Package -> IO Bool
          dependsOn subpkgs p = do
            file <- doesFileExist p
            if file
              then do
              deps <- buildRequires p
              return $ any (`elem` deps) subpkgs
              else return False

merge :: String -> Dist -> [Package] -> IO ()
merge branch =
  repoAction_ True False (git_ "merge" [branch])

missingDeps :: Dist -> [Package] -> IO ()
missingDeps dist =
  repoAction True True checkForMissingDeps dist
  where
    checkForMissingDeps :: Package -> IO ()
    checkForMissingDeps pkg = do
      dir <- takeFileName <$> getCurrentDirectory
      let top = if dir == pkg then ".." else "../.."
          spec = pkg <.> "spec"
      hasSpec <- doesFileExist spec
      if hasSpec
        then do
        deps <- buildRequires (pkg <.> "spec") >>= haskellSrcPkgs top dist
        mapM_ (checkMissing top) deps
        else putStrLn "no spec file found!"
        where
          checkMissing :: FilePath -> Package -> IO ()
          checkMissing top dep = do
            exists <- doesDirectoryExist $ top </> dep
            unless exists $ putStrLn $ "Missing" +-+ dep

oldPackages :: Dist -> [Package] -> IO ()
oldPackages dist pkgs = do
  repopkgs <- repoqueryHaskell True dist
  mapM_ putStrLn (pkgs \\ repopkgs)


prep :: Dist -> [Package] -> IO ()
prep dist =
  repoAction_ True True (cmd_ (rpkg dist) ["prep"]) dist

commit :: String -> Dist -> [Package] -> IO ()
commit logmsg dist =
  repoAction_ True True commitChanges dist
  where
    commitChanges :: IO ()
    commitChanges = do
      -- use gitDiffQuiet
      nochgs <- cmdBool "git" ["diff", "--quiet"]
      if nochgs
        then putStrLn "no changes"
        else cmd_ (rpkg dist) ["commit", "-m", logmsg]

unpushed :: Bool -> Dist -> [Package] -> IO ()
unpushed nolog dist =
  repoAction False True gitLogOneLine dist
  where
    gitLogOneLine :: Package -> IO ()
    gitLogOneLine pkg = do
      out <- git "log" ["origin/" ++ show dist ++ "..HEAD", "--pretty=oneline"]
      unless (null out) $
        putStrLn $ pkg ++ if nolog then "" else (unwords . map replaceHash . words) out
        where
          replaceHash h = if length h /= 40 then h else ":"

verrel :: Dist -> [Package] -> IO ()
verrel dist =
  repoAction_ False True (cmd_ (rpkg dist) ["verrel"]) dist

repoqueryHackageCSV :: Dist -> IO ()
repoqueryHackageCSV dist = do
  pkgs <- repoqueryHaskell False dist
  -- Hackage csv chokes on final newline so remove it
  init . unlines . sort . map (replace "\"ghc-" "\"")  . lines <$> repoquery dist (["--repo=fedora", "--repo=updates", "--latest-limit=1", "--qf=\"%{name}\",\"%{version}\",\"https://src.fedoraproject.org/rpms/%{name}\""] ++ pkgs) >>= putStr

data PkgVer = PV { pvPkg :: String, pvVer :: String}
  deriving (Eq)

instance Show PkgVer
  where
    show (PV p v) = p ++ "-" ++ v

instance Ord PkgVer
  where
    compare (PV p _) (PV p' _) = compare p p'

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b s@(x:xs) =
  if a `isPrefixOf` s
  then b ++ replace a b (drop (length a) s)
  else x:replace a b xs
replace _ _ [] = []

repoqueryHaskell :: Bool -> Dist -> IO [Package]
repoqueryHaskell verbose dist = do
  when verbose $ putStrLn "Getting packages from repoquery"
  let repo = distRepo dist
      updates = maybeToList $ distUpdates dist
  bin <- words <$> repoquery dist (["--repo=" ++ repo] ++ ["--repo=" ++ u | u <- updates] ++ ["--qf=%{source_name}", "--whatrequires", "libHSbase-*-ghc*.so()(64bit)"])
  when (null bin) $ error "No libHSbase consumers found!"
  return $ sort $ nub bin

newPackages :: Dist -> IO [Package]
newPackages dist = do
  ps <- repoqueryHaskell True dist
  kps <- kojiListHaskell True dist
  return $ kps \\ ps

kojiListHaskell :: Bool -> Dist -> IO [Package]
kojiListHaskell verbose dist = do
  when verbose $ putStrLn "Getting package list from Koji"
  libs <- filter (\ p -> "ghc-" `isPrefixOf` p && p `notElem` ["ghc-rpm-macros", "ghc-srpm-macros"]) <$> kojiListPkgs dist
  when (null libs) $ error "No library packages found"
  return $ sort $ nub libs

cloneAllBranches :: Dist -> [Package] -> IO ()
cloneAllBranches _ [] = return ()
cloneAllBranches dist (pkg:rest) = do
  withCurrentDirectory "." $ do
    putStrLn $ "\n==" +-+ pkg +-+ "=="
    -- muser <- getEnv "USER"
    haveSSH <- do
      home <- getHomeDirectory
      doesFileExist $ home </> ".ssh/id_rsa"
    dirExists <- doesDirectoryExist pkg
    unless dirExists $
      cmd_ (rpkg dist) $ ["clone"] ++ ["-a" | not haveSSH] ++ ["-B", pkg]
    singleDir <- isGitDir pkg
    when singleDir $
      error "branch checkout already exists!"
  cloneAllBranches dist rest

repoAction :: Bool -> Bool -> (Package -> IO ()) -> Dist -> [Package] -> IO ()
repoAction _ _ _ _ [] = return ()
repoAction header needsSpec action dist (pkg:rest) = do
  withCurrentDirectory "." $ do
    let branch = distBranch dist
    when header $
      putStrLn $ "\n==" +-+ pkg ++ ":" ++ branch +-+ "=="
    -- muser <- getEnv "USER"
    haveSSH <- do
      home <- getHomeDirectory
      doesFileExist $ home </> ".ssh/id_rsa"
    fileExists <- doesFileExist pkg
    dirExists <- doesDirectoryExist pkg
    if fileExists
      then error $ pkg +-+ "is a file"
      else do
      unless dirExists $
        cmd_ (rpkg dist) $ ["clone"] ++ ["-a" | not haveSSH] ++ ["-b", branch, pkg]
      singleDir <- isGitDir pkg
      unless singleDir $ do
        branchDir <- doesDirectoryExist $ pkg </> branch
        unless branchDir $
          withCurrentDirectory pkg $
          cmd_ (rpkg dist) ["clone", "-b", branch, pkg, branch]
      wd <- pkgDir pkg branch ""
      setCurrentDirectory wd
      pkggit <- do
        gd <- isGitDir "."
        if gd
          then checkPkgsGit
          else return False
      unless pkggit $
        error $ "not a Fedora pkg git dir!:" +-+ wd
      when dirExists $ do
        actual <- gitBranch
        when (branch /= actual) $
          cmd_ (rpkg dist) ["switch-branch", branch]
      isDead <- doesFileExist "dead.package"
      unless isDead $ do
        let spec = pkg <.> "spec"
        hasSpec <- doesFileExist spec
        -- FIXME: silence for cmds that only output package names (eg unpushed -s)
        unless hasSpec $ putStrLn $ (if header then "" else pkg ++ ": ") ++ "No spec file!"
        unless (needsSpec && not hasSpec) $
          action pkg
  repoAction header needsSpec action dist rest

repoAction_ :: Bool -> Bool -> IO () -> Dist -> [Package] -> IO ()
repoAction_ header needsSpec action =
  repoAction header needsSpec (const action)


compareRawhide :: Package -> IO ()
compareRawhide p = do
  let spec = p <.> "spec"
  nvr <- removeDisttag . unwords <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec
  nvr' <- withCurrentDirectory "../master" $ do
    haveSpec <- doesFileExist spec
    unless haveSpec $ cmdSilent "git" ["pull"]
    removeDisttag . unwords <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec
  if nvr == nvr'
    then putStrLn nvr
    else do
    putStrLn nvr
    putStrLn nvr'
  putStrLn ""
  where
    removeDisttag = reverse . tail . dropWhile (/= '.') . reverse

isFromHackage :: Package -> IO Bool
isFromHackage pkg =
  grep_ "hackage.haskell.org/package/" $ pkg <.> "spec"


updateOrRefreshPackage :: Bool -> Package -> IO ()
updateOrRefreshPackage refresh pkg = do
  hckg <- isFromHackage pkg
  let mode = if refresh then "refresh" else "update"
  if hckg
    then cmd_ "cabal-rpm" [mode]
    else putStrLn "skipping since not hackage"

