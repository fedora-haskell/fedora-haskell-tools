-- |
-- Module      :  Packages
-- Copyright   :  (C) 2017-2019  Jens Petersen
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

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,13,0))
#else
import Control.Applicative (optional, some, (<|>)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
                           , pure, (<$>), (<*>)
#endif
                           )
#endif
import Control.Monad (filterM, unless, when, (>=>))
import Data.Maybe
import Data.List (intercalate, isInfixOf, isPrefixOf, nub, sort, (\\))

import Data.List.Split (splitOn)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getHomeDirectory,
#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
                         listDirectory,
#else
                         getDirectoryContents,
#endif
                         setCurrentDirectory)
import System.FilePath ((</>), (<.>), takeFileName)
import System.IO (BufferMode(..), hIsTerminalDevice, hSetBuffering, stdout)
--import System.Posix.Env (getEnv)
import Text.CSV (parseCSV)

import FedoraDists (Dist(..), distBranch, distRepo, distUpdates, rawhide)

import SimpleCmd ((+-+), cmd, cmd_, cmdBool, cmdLines, cmdMaybe, cmdQuiet,
                  cmdSilent, grep_, removePrefix, shell_, warning)
import SimpleCmd.Git (git, git_, gitBranch, gitDiffQuiet, isGitDir)
import SimpleCmdArgs

import Build (build, readBuildCmd)
import Dist (distArg, distRemote, hackageRelease, ltsStream)
import Koji (kojiListPkgs, rpkg)
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
  cwd <- getCurrentDirectory
  simpleCmdArgs (Just version) "Fedora Haskell packages tool"
    "Fedora packages maintenance tool" $
    subcommands $
    [ Subcommand "checkout" "fedpkg switch-branch" $
      repoAction_ True False (return ()) <$> distArg <*> pkgArgs
    , Subcommand "clone"  "clone repos" $
      clone <$> branching <*> distArg <*> pkgArgs
    , Subcommand "clone-new" "clone new packages" $
      cloneNew <$> branching <*> distArg
    , Subcommand "cblrpm" "Run cblrpm command" $
      cblrpm <$> strOptionWith 'c' "cmd" "CMD" "command to execute" <*> distArg <*> pkgArgs
    , Subcommand "cmd" "arbitrary command (with args)" $
      execCmd <$> strOptionWith 'c' "cmd" "CMD" "command to execute" <*> distArg <*> pkgArgs
    , Subcommand "count" "count number of packages" $
      (repoqueryHaskellPkgs False >=> (print . length)) <$> distArg
    , Subcommand "depends" "cabal-depends" $
      repoAction False (Output cabalDepends) <$> distArg <*> pkgArgs
    , Subcommand "diff" "git diff" $
      gitDiff <$> optional gitFormat
      <*> optional (strOptionWith 'w' "with-branch" "BRANCH" "Branch to compare")
      <*> distArg <*> pkgArgs
    , Subcommand "diff-origin" "git diff origin" $
      gitDiffOrigin <$> distArg <*> pkgArgs
    , Subcommand "diff-branch" "compare branch with master" $
      repoAction True (Header False compareRawhide) <$> distArg <*> pkgArgs
    , Subcommand "diffstat" "Show diffstat output" $
      repoAction False (Output (const diffStat)) <$> distArg <*> pkgArgs
    , Subcommand "hackage" "generate Hackage distro data" $
      repoqueryHackageCSV hackageRelease <$> switchRefresh
    , Subcommand "hackage-compare" "compare with Hackage distro data" $
      hackageCompare <$> switchRefresh
    , Subcommand "head-origin" "packages with head in sync with origin" $
      headOrigin  <$> distArg <*> pkgArgs
    , Subcommand "leaf" "list leaf packages (slow!)" $
      leaves <$> switchWith 'v' "deps" "show also deps" <*> distArg <*> pkgArgs
    , Subcommand "list" "list packages that BR ghc-Cabal-devel" $
      (repoqueryHaskellPkgs False >=> putStrList) <$> distArg
    , Subcommand "merge" "git merge" $
      merge <$> strOptionWith 'f' "from" "BRANCH" "specify branch to merge from" <*> distArg <*> pkgArgs
    , Subcommand "missing" "missing dependency source packages" $
      missingDeps <$> distArg <*> pkgArgs
    , Subcommand "new" "unbuilt packages" $
      (newPackages >=> putStrList) <$> distArg
    , Subcommand "old-packages" "packages not in repoquery" $
      oldPackages <$> distArg <*> pkgArgs
    , Subcommand "prep" "fedpkg prep" $
      prep <$> distArg <*> pkgArgs
    , Subcommand "stackage-compare" "compare with stackage" $
      stackageCompare <$> streamOpt <*> switchWith 'm' "missing" "only list missing packages" <*> distArg <*> pkgArgs
    , Subcommand "commit" "fedpkg commit" $
      commit <$> strOptionWith 'm' "message" "COMMITMSG" "commit message" <*> distArg <*> pkgArgs
    , Subcommand "fetch" "git fetch repos" $
      repoAction_ True False (git_ "fetch" []) <$> distArg <*> pkgArgs
    , Subcommand "pull" "git pull repos" $
      repoAction_ True False (git_ "pull" ["--rebase"]) <$> distArg <*> pkgArgs
    , Subcommand "push" "git push repos" $
      repoAction_ True False (git_ "push" []) <$> distArg <*> pkgArgs
    , Subcommand "refresh" "cabal-rpm refresh" $
      refresh <$> switchWith 'n' "dry-run" "Show patch but don't apply" <*> distArg <*> pkgArgs
    , Subcommand "remaining" "remaining packages to be built in TAG" $
      remaining <$> switchWith 'c' "count" "show many packages left" <*> strArg "TAG" <*> pkgArgs
    , Subcommand "subpkgs" "list subpackages" $
      repoAction True (Header True (\ p -> rpmspec [] (Just "%{name}-%{version}") (p <.> "spec") >>= putStrList)) <$> distArg <*> pkgArgs
    , Subcommand "tagged" "list koji DIST tagged builds" $
      listTagged_ <$> switchWith 's' "short" "list packages not builds" <*> strArg "TAG"
    , Subcommand "unpushed" "show unpushed commits" $
      unpushed <$> switchWith 's' "short" "no log" <*> distArg <*> pkgArgs
    , Subcommand "update" "cabal-rpm update" $
      update <$> streamOpt <*> distArg <*> pkgArgs
    , Subcommand "verrel" "show nvr of packages" $
      verrel <$> distArg <*> pkgArgs] ++
    map (buildCmd cwd) [ ("install", "build locally and install")
                       , ("mock", "build in mock")
                       , ("chain", "build deps recursively in Koji")
                       , ("koji", "build in Koji (deprecated)")
                       , ("pending", "show planned changes")
                       , ("changed", "show changed pkgs")
                       , ("built", "show pkgs whose NVR already built")
                       , ("bump", "bump release for NVRs already built")
                       , ("not-installed", "list packages not locally installed")
                       ]
  where
    pkgArgs = some (strArg "PKG...")

    branching = switchWith 'B' "branches" "clone branch dirs (fedpkg clone -B)"

    gitFormat :: Parser DiffFormat
    gitFormat =
      flagWith' DiffShort 's' "short" "Just output package name" <|>
      DiffContext <$> optionWith auto 'u' "unified" "CONTEXT" "Lines of context"

    buildCmd cwd (c, desc) =
      Subcommand c desc  $
      build cwd Nothing Nothing False (readBuildCmd c) <$> distArg <*> pkgArgs

    switchRefresh = switchWith 'r' "refresh" "repoquery --refresh"

    streamOpt = strOptionalWith 's' "stream" "STACKAGESTREAM" ("Stackage stream [" ++ ltsStream ++ "]") ltsStream

data DiffFormat =
  DiffShort | DiffContext Int
  deriving (Eq)

putStrList :: [String] -> IO ()
putStrList =
  putStr . unlines

-- should make separate rhel client so -B does not need dist
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
  repoAction_ True True (shell_ cs) dist pkgs

gitDiff :: Maybe DiffFormat -> Maybe String -> Dist -> [Package] -> IO ()
gitDiff fmt mbrnch =
  repoAction False (Output doGitDiff)
  where
    doGitDiff pkg = do
      let branch = maybeToList mbrnch
          contxt = case fmt of
                     (Just (DiffContext n)) -> ["-u", show n]
                     _ -> []
          short = fmt == Just DiffShort
      out <- git "diff" $ branch ++ contxt
      return $ if null out then ""
               else if short then pkg else out

gitDiffOrigin :: Dist -> [Package] -> IO ()
gitDiffOrigin dist =
  repoAction False (Output (const (git "diff" [distRemote dist]))) dist

stackageCompare :: String -> Bool -> Dist -> [Package] -> IO ()
stackageCompare stream missingOnly dist =
  repoAction True (Header False compareStackage) dist
  where
    compareStackage :: Package -> IO ()
    compareStackage p = do
      nvr <- cmd (rpkg dist) ["verrel"]
      stkg <- cmdMaybe "stackage" ["package", stream, removePrefix "ghc-" p]
      let same = isJust stkg && (fromJust stkg ++ "-") `isInfixOf` nvr
      unless same $
        if missingOnly
        then when (isNothing stkg) $ putStrLn p
        else
          if isNothing stkg
          then putStrLn $ stream ++ " missing: " ++ removePrefix "ghc-" p
          else do
            putStrLn nvr
            putStrLn $ replicate (length (dropVerrel nvr) + 1) ' ' ++ fromJust stkg +-+ "(" ++ stream ++ ")"

diffStat :: IO String
diffStat = git "diff" ["--stat"]

hackageCompare :: Bool -> IO ()
hackageCompare refreshData =
  repoqueryHackages hackageRelease >>=
  compareHackage hackageRelease
  where
    compareHackage :: Dist -> [Package] -> IO ()
    compareHackage dist pkgs' = do
      hck <- simpleHTTP (getRequest "http://hackage.haskell.org/distro/Fedora/packages.csv") >>= getResponseBody
      let hackage = sort . either (error "Malformed Hackage csv") (map mungeHackage) $ parseCSV "packages.csv" hck
      sort . map mungeRepo . lines <$> repoquery dist (["--repo=fedora", "--repo=updates", "--latest-limit=1", "--qf=%{name},%{version}"] ++ ["--refresh" | refreshData] ++ pkgs') >>=
        compareSets True hackage

    mungeHackage :: [String] -> PkgVer
    mungeHackage [n,v,_] = PV n v
    mungeHackage _ = error "Malformed Hackage csv"

    mungeRepo :: String -> PkgVer
    mungeRepo s | ',' `elem` s =
                  let (p,v) = break (== ',') s in
                    PV (removePrefix "ghc-" p) (tail v)
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
  repoAction False (Header False gitHeadAtOrigin) dist
  where
    gitHeadAtOrigin :: Package -> IO ()
    gitHeadAtOrigin pkg = do
      -- use gitDiffQuiet
      same <- cmdBool "git" ["diff", "--quiet", distRemote dist ++ "..HEAD"]
      when same $ putStrLn pkg

leaves :: Bool -> Dist -> [Package] -> IO ()
leaves verb =
  repoAction True (Header verb checkLeafPkg)
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
        else when verb $ putStrList found
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
  repoAction True (Output checkForMissingDeps) dist
  where
    checkForMissingDeps :: Package -> IO String
    checkForMissingDeps pkg = do
      dir <- takeFileName <$> getCurrentDirectory
      let top = if dir == pkg then ".." else "../.."
          spec = pkg <.> "spec"
      hasSpec <- doesFileExist spec
      if hasSpec
        then do
        deps <- buildRequires (pkg <.> "spec") >>= haskellSrcPkgs top dist
        unlines <$> filterM (noPkgDir top) deps
        else putStrLn ("no " ++ pkg ++ ".spec file found!") >> return ""
        where
          noPkgDir :: FilePath -> Package -> IO Bool
          noPkgDir top dep =
            not <$> doesDirectoryExist (top </> dep)

oldPackages :: Dist -> [Package] -> IO ()
oldPackages dist pkgs = do
  repopkgs <- repoqueryHaskellPkgs True dist
  putStrList (pkgs \\ repopkgs)

prep :: Dist -> [Package] -> IO ()
prep dist =
  repoAction_ True True (cmd_ (rpkg dist) ["prep"]) dist

commit :: String -> Dist -> [Package] -> IO ()
commit logmsg dist =
  repoAction False (Output (const commitChanges)) dist
  where
    commitChanges :: IO String
    commitChanges = do
      nochgs <- gitDiffQuiet []
      if nochgs
        then return ""
        else cmd (rpkg dist) ["commit", "-m", logmsg]

unpushed :: Bool -> Dist -> [Package] -> IO ()
unpushed nolog dist =
  repoAction True (Header False gitLogOneLine) dist
  where
    gitLogOneLine :: Package -> IO ()
    gitLogOneLine pkg = do
      out <- git "log" [distRemote dist ++ "..HEAD", "--pretty=oneline"]
      unless (null out) $
        putStrLn $ pkg ++ if nolog then "" else (unwords . map replaceHash . words) out
        where
          replaceHash h = if length h /= 40 then h else ":"

verrel :: Dist -> [Package] -> IO ()
verrel dist =
  repoAction_ False True (cmd_ (rpkg dist) ["verrel"]) dist

repoqueryHackageCSV :: Dist -> Bool -> IO ()
repoqueryHackageCSV dist refreshData = do
  pkgs <- repoqueryHackages dist
  -- Hackage csv chokes on final newline so remove it
  init . unlines . sort . map (replace "\"ghc-" "\"")  . lines <$> repoquery dist (["--repo=fedora", "--repo=updates", "--latest-limit=1", "--qf=\"%{name}\",\"%{version}\",\"https://apps.fedoraproject.org/packages/%{source_name}\""] ++ ["--refresh" | refreshData] ++ pkgs) >>= putStr

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

repoqueryHaskellPkgs :: Bool -> Dist -> IO [Package]
repoqueryHaskellPkgs verbose dist = do
  when verbose $ do
    tty <- hIsTerminalDevice stdout
    when tty $ warning "Getting packages from repoquery"
  let repo = distRepo dist
      updates = maybeToList $ distUpdates dist
  bin <- words <$> repoquery dist (["--repo=" ++ repo ++ "-source"] ++ ["--repo=" ++ u  ++ "-source" | u <- updates] ++ ["--qf=%{name}", "--whatrequires", "ghc-Cabal-*"])
  when (null bin) $ error "No packages using ghc-Cabal-devel found!"
  return $ sort $ nub bin

repoqueryHackages :: Dist -> IO [Package]
repoqueryHackages dist = do
  srcs <- repoqueryHaskellPkgs False dist
  libs <- repoqueryHaskellLibs False
  let binsrcs = filter (not . ("ghc-" `isPrefixOf`)) srcs
      sublibs = libs \\ map ("ghc-" ++) binsrcs
  return $ sort $ nub (srcs ++ sublibs)
  where
    repoqueryHaskellLibs :: Bool -> IO [Package]
    repoqueryHaskellLibs verbose = do
      when verbose $ putStrLn "Getting libraries from repoquery"
      let repo = distRepo dist
          updates = maybeToList $ distUpdates dist
      bin <- words <$> repoquery dist (["--repo=" ++ repo] ++ ["--repo=" ++ u | u <- updates] ++ ["--qf=%{name}", "--whatprovides", "libHS*-ghc*.so()(64bit)"])
      when (null bin) $ error "No libHS*.so providers found!"
      return $ sort $ nub bin

newPackages :: Dist -> IO [Package]
newPackages dist = do
  ps <- repoqueryHaskellPkgs True dist
  pps <- cmdLines "pagure" ["list", "ghc*"]
  filterM (\ d -> not <$> doesFileExist (d </> "dead.package")) $ pps \\ (ps ++ ["ghc", "ghc-rpm-macros", "ghc-srpm-macros"])

kojiListHaskell :: Bool -> Dist -> IO [Package]
kojiListHaskell verbose dist = do
  when verbose $ putStrLn "Getting package list from Koji"
  libs <- filter (\ p -> "ghc" `isPrefixOf` p && p `notElem` ["ghc-rpm-macros", "ghc-srpm-macros"]) <$> kojiListPkgs dist
  when (null libs) $ error "No library packages found"
  return $ sort $ nub libs

haveSshKey :: IO Bool
haveSshKey = do
  home <- getHomeDirectory
  doesFileExist $ home </> ".ssh/id_rsa"

cloneAllBranches :: Dist -> [Package] -> IO ()
cloneAllBranches _ [] = return ()
cloneAllBranches dist (pkg:rest) = do
  withCurrentDirectory "." $ do
    putStrLn $ "\n==" +-+ pkg +-+ "=="
    -- muser <- getEnv "USER"
    haveSSH <- haveSshKey
    dirExists <- doesDirectoryExist pkg
    unless dirExists $
      cmd_ (rpkg dist) $ ["clone"] ++ ["-a" | not haveSSH] ++ ["-B", pkg]
    singleDir <- isGitDir pkg
    when singleDir $
      error "branch checkout already exists!"
  cloneAllBranches dist rest

data Action = Output (Package -> IO String) | Header Bool (Package -> IO ())

showHeader :: Action -> Bool
showHeader (Header b _) = b
showHeader (Output _) = False

repoAction :: Bool -> Action -> Dist -> [Package] -> IO ()
repoAction _ _ _ [] = return ()
repoAction needsSpec action dist (pkg:rest) = do
  withCurrentDirectory "." $ do
    let branch = distBranch dist
    when (showHeader action) $
      putStrLn $ "\n==" +-+ pkg ++ ":" ++ branch +-+ "=="
    -- muser <- getEnv "USER"
    haveSSH <- haveSshKey
    fileExists <- doesFileExist pkg
    if fileExists
      then error $ pkg +-+ "is a file"
      else do
      dirExists <- doesDirectoryExist pkg
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
        unless hasSpec $ putStrLn $ (if showHeader action then "" else pkg ++ ": ") ++ "No spec file!"
        unless (needsSpec && not hasSpec) $
          case action of
            Header _ act -> act pkg
            Output act -> do
              out <- act pkg
              unless (null out) $ do
                putStrLn $ "\n==" +-+ pkg ++ ":" ++ branch +-+ "=="
                putStrLn out
  repoAction needsSpec action dist rest

-- io independent of package
repoAction_ :: Bool -> Bool -> IO () -> Dist -> [Package] -> IO ()
repoAction_ header needsSpec action =
  repoAction needsSpec (Header header (const action))

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

update :: String -> Dist -> [Package] -> IO ()
update stream =
  repoAction True (Header True doUpdate)
  where
    doUpdate :: Package -> IO ()
    doUpdate pkg = do
      hckg <- isFromHackage pkg
      if hckg
        then cmd_ "cabal-rpm" ["update", "-s", stream]
        else putStrLn "skipping since not hackage"

refresh :: Bool -> Dist -> [Package] -> IO ()
refresh dryrun =
  repoAction True (Header True refreshPkg)
  where
    refreshPkg :: Package -> IO ()
    refreshPkg pkg = do
      hckg <- isFromHackage pkg
      if hckg
        then cmd_ "cabal-rpm" $ "refresh" : ["--dry-run" | dryrun]
        else putStrLn "skipping since not hackage"

listTagged_ :: Bool -> String -> IO ()
listTagged_ short tag =
  listTagged short tag >>= putStrList

listTagged :: Bool -> String -> IO [String]
listTagged short tag = do
  builds <- map (head . words) <$> cmdLines "koji" ["list-tagged", "--quiet", tag]
  return $ nub $ map (if short then dropVerrel else id) builds

dropVerrel :: String -> String
dropVerrel nvr =
  let parts = splitOn "-" nvr in
    intercalate "-" $ take (length parts - 2) parts

remaining :: Bool -> String -> [Package] -> IO ()
remaining count tag pkgs = do
  built <- listTagged True tag
  let left = pkgs \\ built
  if count
    then print $ length left
    else cmd_ "rpmbuild-order" $ ["sort", "-p"] ++ left

cabalDepends :: Package -> IO String
cabalDepends p = do
  hckg <- isFromHackage p
  if hckg then do
    vr <- removePrefix "ghc-" . head <$>
      rpmspec ["--srpm"] (Just "%{name}-%{version}") (p <.> "spec")
    setCurrentDirectory vr
    cmdQuiet "cabal-depends" ["--not-build", "--unique"]
    else return ""

cblrpm :: String -> Dist -> [Package] -> IO ()
cblrpm "" = error "CMD string must be given"
cblrpm cs =
  repoAction True (Header True doCblRpm)
  where
    doCblRpm :: Package -> IO ()
    doCblRpm p = do
    hckg <- isFromHackage p
    when hckg $
      cmd_ "cblrpm" [cs]
