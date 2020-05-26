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
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Data.List (intercalate, isInfixOf, isPrefixOf, nub, sort, (\\))

import Data.List.Split (splitOn)
import qualified Network.HTTP as H
import Network.HTTP.Simple
import Network.HTTP.Types
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

import Distribution.Fedora (Dist(..), distBranch, distRepo, distUpdates,
                            getLatestFedoraDist, getRawhideDist)
--import Distribution.Fedora.Branch (Branch(..))

import SimpleCmd ((+-+), cmd, cmd_, cmdLines, cmdMaybe, cmdQuiet, {-cmdSilent,-}
                  grep_, removePrefix, removeSuffix, shell_, warning)
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
  hSetBuffering stdout NoBuffering
  cwd <- getCurrentDirectory
  branched <- getLatestFedoraDist
  simpleCmdArgs (Just version) "Fedora Haskell packages tool"
    "Fedora packages maintenance tool" $
    subcommands . sort $
    [ Subcommand "checkout" "fedpkg switch-branch" $
      repoAction_ branched True False (return ()) <$> distArg <*> pkgArgs
    , Subcommand "clone"  "clone repos" $
      clone branched <$> branching <*> distArg <*> pkgArgs
    , Subcommand "clone-new" "clone new packages" $
      cloneNew branched <$> branching <*> distArg
    , Subcommand "cblrpm" "Run cblrpm command" $
      cblrpm branched <$> strOptionWith 'c' "cmd" "CMD" "command to execute" <*> distArg <*> pkgArgs
    , Subcommand "cmd" "arbitrary command (with args)" $
      execCmd branched <$> strOptionWith 'c' "cmd" "CMD" "command to execute" <*> distArg <*> pkgArgs
    , Subcommand "count" "count number of packages" $
      (repoqueryHaskellPkgs branched False >=> (print . length)) <$> distArg
    , Subcommand "depends" "cabal-depends" $
      repoAction branched False (Output cabalDepends) <$> distArg <*> pkgArgs
    , Subcommand "diff" "git diff" $
      gitDiff branched <$> optional gitFormat
      <*> optional (strOptionWith 'w' "with-branch" "BRANCH" "Branch to compare")
      <*> distArg <*> pkgArgs
    , Subcommand "diff-origin" "git diff origin" $
      gitDiffOrigin branched <$> distArg <*> pkgArgs
--    , Subcommand "diff-branch" "compare branch with master" $
--      repoAction branched True (Header False compareRawhide) <$> distArg <*> pkgArgs
    , Subcommand "diffstat" "Show diffstat output" $
      repoAction branched False (Output (const diffStat)) <$> distArg <*> pkgArgs
    , Subcommand "hackage-upload" "upload Hackage distro data" $
      hackageUpload branched <$> switchRefresh
    , Subcommand "hackage-compare" "compare with Hackage distro data" $
      hackageCompare branched <$> switchRefresh
    -- more or less the same as 'pushed'
    , Subcommand "head-origin" "packages with head in sync with origin" $
      headOrigin branched <$> distArg <*> pkgArgs
    , Subcommand "leaf" "list leaf packages (slow!)" $
      leaves branched <$> switchWith 'v' "deps" "show also deps" <*> distArg <*> pkgArgs
    , Subcommand "list" "list packages that BR ghc-Cabal-devel" $
      (repoqueryHaskellPkgs branched False >=> putStrList) <$> distArg
    , Subcommand "merge" "git merge" $
      merge branched <$> strOptionWith 'f' "from" "BRANCH" "specify branch to merge from" <*> distArg <*> pkgArgs
    , Subcommand "missing" "missing dependency source packages" $
      missingDeps branched <$> distArg <*> pkgArgs
    , Subcommand "new" "unbuilt packages" $
      (newPackages branched >=> putStrList) <$> distArg
    , Subcommand "old-packages" "packages not in repoquery" $
      oldPackages branched <$> distArg <*> pkgArgs
    , Subcommand "prep" "fedpkg prep" $
      prep branched <$> distArg <*> pkgArgs
    , Subcommand "stackage-compare" "compare with stackage" $
      stackageCompare branched <$> streamOpt <*> stackageOpts <*> distArg <*> pkgArgs
    , Subcommand "commit" "fedpkg commit" $
      commit branched <$> strOptionWith 'm' "message" "COMMITMSG" "commit message" <*> distArg <*> pkgArgs
    , Subcommand "fetch" "git fetch repos" $
      repoAction_ branched True False (git_ "fetch" []) <$> distArg <*> pkgArgs
    , Subcommand "pull" "git pull repos" $
      repoAction_ branched True False (git_ "pull" ["--rebase"]) <$> distArg <*> pkgArgs
    , Subcommand "push" "git push repos" $
      repoAction_ branched True False (git_ "push" []) <$> distArg <*> pkgArgs
    , Subcommand "pushed" "show git pushed packages" $
      pushed branched <$> distArg <*> pkgArgs
    , Subcommand "refresh" "cabal-rpm refresh" $
      refresh branched <$> switchWith 'n' "dry-run" "Show patch but don't apply" <*> distArg <*> pkgArgs
    , Subcommand "remaining" "remaining packages to be built in TAG" $
      remaining <$> switchWith 'c' "count" "show many packages left" <*> strArg "TAG" <*> pkgArgs
    , Subcommand "subpkgs" "list subpackages" $
      repoAction branched True (Header True (\ p -> rpmspec [] (Just "%{name}-%{version}") (p <.> "spec") >>= putStrList)) <$> distArg <*> pkgArgs
    , Subcommand "tagged" "list koji DIST tagged builds" $
      listTagged_ <$> switchWith 's' "short" "list packages not builds" <*> strArg "TAG"
    , Subcommand "unbranched" "packages without this branch" $
      unbranched branched <$> distArg <*> pkgArgs
    , Subcommand "unpushed" "show unpushed commits" $
      unpushed branched <$> switchWith 's' "short" "no log" <*> distArg <*> pkgArgs
    , Subcommand "update" "cabal-rpm update" $
      update branched <$> streamOpt <*> distArg <*> pkgArgs
    , Subcommand "verrel" "show nvr of packages" $
      verrel branched <$> distArg <*> pkgArgs] ++
    map (buildCmd cwd) [ ("install", "build locally and install")
                       , ("mock", "build in mock")
                       , ("chain", "build deps recursively in Koji")
                       , ("koji", "build in Koji (deprecated) without checking dependencies")
                       , ("pending", "show planned changes")
                       , ("changed", "show changed pkgs")
                       , ("built", "show pkgs whose NVR already built")
                       , ("bump", "bump release for NVRs already built")
                       , ("not-installed", "list packages not locally installed")
                       ]
  where
    pkgArgs = some (removeSuffix "/" <$> strArg "PKG...")

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

    stackageOpts :: Parser StkgOpt
    stackageOpts =
      flagWith' StkgMissing 'm' "missing" "only list missing packages" <|>
      flagWith StkgAll StkgOnly 'o' "only" "only Stackage packages"

data DiffFormat =
  DiffShort | DiffContext Int
  deriving (Eq)

data StkgOpt = StkgAll | StkgOnly | StkgMissing
  deriving Eq

putStrList :: [String] -> IO ()
putStrList =
  putStr . unlines

-- should make separate rhel client so -B does not need dist
clone :: Dist -> Bool -> Dist -> [Package] -> IO ()
clone _ True dist pkgs = cloneAllBranches dist pkgs
clone branched False dist pkgs =
  repoAction_ branched True False (return ()) dist pkgs

cloneNew :: Dist -> Bool -> Dist -> IO ()
cloneNew branched True dist = do
  rawhide <- getRawhideDist
  newPackages branched rawhide >>= cloneAllBranches dist
cloneNew branched False dist =
  newPackages branched dist >>= repoAction_ branched True False (return ()) dist

execCmd :: Dist -> String -> Dist -> [Package] -> IO ()
execCmd _ "" _ _ = error "CMD string must be given"
execCmd branched cs dist pkgs =
  repoAction_ branched True True (shell_ cs) dist pkgs

gitDiff :: Dist -> Maybe DiffFormat -> Maybe String -> Dist -> [Package] -> IO ()
gitDiff branched (Just DiffShort) mbrnch =
  repoAction branched False (Header False doGitDiff)
  where
    doGitDiff pkg = do
      let branch = maybeToList mbrnch
      out <- git "diff" branch
      unless (null out) $ putStrLn pkg
gitDiff branched fmt mbrnch =
  repoAction branched False (Output (const doGitDiff))
  where
    doGitDiff = do
      let branch = maybeToList mbrnch
          contxt = case fmt of
                     (Just (DiffContext n)) -> ["--unified=" ++ show n]
                     _ -> []
      out <- git "diff" $ branch ++ contxt
      return $ if null out then "" else out

gitDiffOrigin :: Dist -> Dist -> [Package] -> IO ()
gitDiffOrigin branched dist =
  repoAction branched False (Output (const (git "diff" [distRemote branched dist]))) dist

stackageCompare :: Dist -> String -> StkgOpt -> Dist -> [Package] -> IO ()
stackageCompare branched stream opt dist =
  repoAction branched True (Header False compareStackage) dist
  where
    compareStackage :: Package -> IO ()
    compareStackage p = do
      nvr <- cmd (rpkg dist) ["verrel"]
      stkg <- cmdMaybe "stackage" ["package", stream, removePrefix "ghc-" p]
      let same = isJust stkg && (fromJust stkg ++ "-") `isInfixOf` nvr
      unless same $
        if opt == StkgMissing
        then when (isNothing stkg) $ putStrLn p
        else
          if isNothing stkg
          then unless (opt == StkgOnly) $
               putStrLn $ stream ++ " missing: " ++ removePrefix "ghc-" p
          else do
            putStrLn nvr
            putStrLn $ replicate (length (dropVerrel nvr) + 1) ' ' ++ fromJust stkg +-+ "(" ++ stream ++ ")"

diffStat :: IO String
diffStat = git "diff" ["--stat"]

hackageUpload :: Dist -> Bool -> IO ()
hackageUpload branched refreshData = do
  csv <- repoqueryHackageCSV hackageRelease
  home <- getHomeDirectory
  [username, password] <- map B.pack . words <$> readFile (home </> ".fedora/hackage.auth")
  req <- setRequestBasicAuth username password .
         setRequestBodyLBS (BL.pack csv) .
         addRequestHeader hContentType (B.pack "text/csv") .
         setRequestMethod methodPut <$>
         parseRequestThrow "https://hackage.haskell.org/distro/Fedora/packages.csv"
  resp <- httpLbs req
  BL.putStrLn $ getResponseBody resp
  where
    repoqueryHackageCSV :: Dist -> IO String
    repoqueryHackageCSV dist = do
      pkgs <- repoqueryHackages branched hackageRelease
      -- Hackage csv chokes on a final newline
      intercalate "\n" . sort . map (replace "\"ghc-" "\"")  . lines <$> repoquery dist (["--repo=fedora", "--repo=updates", "--latest-limit=1", "--qf=\"%{name}\",\"%{version}\",\"https://src.fedoraproject.org/rpms/%{source_name}\""] ++ ["--refresh" | refreshData] ++ pkgs)

hackageCompare :: Dist -> Bool -> IO ()
hackageCompare branched refreshData =
  repoqueryHackages branched hackageRelease >>=
  compareHackage hackageRelease
  where
    compareHackage :: Dist -> [Package] -> IO ()
    compareHackage dist pkgs' = do
      hck <- H.simpleHTTP (H.getRequest "http://hackage.haskell.org/distro/Fedora/packages.csv") >>= H.getResponseBody
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

headOrigin :: Dist -> Dist -> [Package] -> IO ()
headOrigin branched dist =
  repoAction branched False (Header False gitHeadAtOrigin) dist
  where
    gitHeadAtOrigin :: Package -> IO ()
    gitHeadAtOrigin pkg = do
      -- use gitDiffQuiet
      same <- gitBool "diff" ["--quiet", distRemote branched dist ++ "..HEAD"]
      when same $ putStrLn pkg

leaves :: Dist -> Bool -> Dist -> [Package] -> IO ()
leaves branched verb =
  repoAction branched True (Header verb checkLeafPkg)
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

merge :: Dist -> String -> Dist -> [Package] -> IO ()
merge branched branch =
  repoAction_ branched True False (git_ "merge" [branch])

missingDeps :: Dist -> Dist -> [Package] -> IO ()
missingDeps branched dist =
  repoAction branched True (Output checkForMissingDeps) dist
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

oldPackages :: Dist -> Dist -> [Package] -> IO ()
oldPackages branched dist pkgs = do
  repopkgs <- repoqueryHaskellPkgs branched True dist
  putStrList (pkgs \\ repopkgs)

prep :: Dist -> Dist -> [Package] -> IO ()
prep branched dist =
  repoAction_ branched True True (cmd_ (rpkg dist) ["prep"]) dist

commit :: Dist -> String -> Dist -> [Package] -> IO ()
commit branched logmsg dist =
  repoAction branched False (Output (const commitChanges)) dist
  where
    commitChanges :: IO String
    commitChanges = do
      nochgs <- gitDiffQuiet []
      if nochgs
        then return ""
        else cmd (rpkg dist) ["commit", "-m", logmsg]

unpushed :: Dist -> Bool -> Dist -> [Package] -> IO ()
unpushed branched nolog dist =
  repoAction branched True (Header False gitLogOneLine) dist
  where
    gitLogOneLine :: Package -> IO ()
    gitLogOneLine pkg = do
      out <- git "log" [distRemote branched dist ++ "..HEAD", "--pretty=oneline"]
      unless (null out) $
        putStrLn $ pkg ++ if nolog then "" else (unwords . map replaceHash . words) out
        where
          replaceHash h = if length h /= 40 then h else ":"

pushed :: Dist -> Dist -> [Package] -> IO ()
pushed branched dist =
  repoAction branched True (Header False checkPushed) dist
  where
    checkPushed :: Package -> IO ()
    checkPushed pkg = do
      out <- git "log" [distRemote branched dist ++ "..HEAD", "--pretty=oneline"]
      when (null out) $ putStrLn pkg

verrel :: Dist -> Dist -> [Package] -> IO ()
verrel branched dist =
  repoAction_ branched False True (cmd_ (rpkg dist) ["verrel"]) dist

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

repoqueryHaskellPkgs :: Dist -> Bool -> Dist -> IO [Package]
repoqueryHaskellPkgs branched verbose dist = do
  when verbose $ do
    tty <- hIsTerminalDevice stdout
    when tty $ warning "Getting packages from repoquery"
  let repo = distRepo branched dist
      updates = maybeToList $ distUpdates branched dist
  bin <- words <$> repoquery dist (["--repo=" ++ repo ++ "-source"] ++ ["--repo=" ++ u  ++ "-source" | u <- updates] ++ ["--qf=%{name}", "--whatrequires", "ghc-Cabal-*"])
  when (null bin) $ error "No packages using ghc-Cabal-devel found!"
  return $ sort $ nub bin

repoqueryHackages :: Dist -> Dist -> IO [Package]
repoqueryHackages branched dist = do
  srcs <- repoqueryHaskellPkgs branched False dist
  libs <- repoqueryHaskellLibs False
  let binsrcs = filter (not . ("ghc-" `isPrefixOf`)) srcs
      sublibs = libs \\ map ("ghc-" ++) binsrcs
  return $ sort $ nub (srcs ++ sublibs)
  where
    repoqueryHaskellLibs :: Bool -> IO [Package]
    repoqueryHaskellLibs verbose = do
      when verbose $ putStrLn "Getting libraries from repoquery"
      let repo = distRepo branched dist
          updates = maybeToList $ distUpdates branched dist
      bin <- words <$> repoquery dist (["--repo=" ++ repo] ++ ["--repo=" ++ u | u <- updates] ++ ["--qf=%{name}", "--whatprovides", "libHS*-ghc*.so()(64bit)"])
      when (null bin) $ error "No libHS*.so providers found!"
      return $ sort $ nub bin

newPackages :: Dist -> Dist -> IO [Package]
newPackages branched dist = do
  ps <- repoqueryHaskellPkgs branched True dist
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

repoAction :: Dist -> Bool -> Action -> Dist -> [Package] -> IO ()
repoAction _ _ _ _ [] = return ()
repoAction branched needsSpec action dist (pkg:rest) = do
  withCurrentDirectory "." $ do
    let branch = distBranch branched dist
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
  repoAction branched needsSpec action dist rest

-- io independent of package
repoAction_ :: Dist -> Bool -> Bool -> IO () -> Dist -> [Package] -> IO ()
repoAction_ branched header needsSpec action =
  repoAction branched needsSpec (Header header (const action))

-- compareRawhide :: Package -> IO ()
-- compareRawhide p = do
--   let spec = p <.> "spec"
--   nvr <- removeDisttag . unwords <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec
--   nvr' <- withBranch "master" $ do
--     haveSpec <- doesFileExist spec
--     unless haveSpec $ cmdSilent "git" ["pull"]
--     removeDisttag . unwords <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec
--   if nvr == nvr'
--     then putStrLn nvr
--     else do
--     putStrLn nvr
--     putStrLn nvr'
--   putStrLn ""
--   where
--     removeDisttag = reverse . tail . dropWhile (/= '.') . reverse

isFromHackage :: Package -> IO Bool
isFromHackage pkg =
  grep_ "hackage.haskell.org/package/" $ pkg <.> "spec"

update :: Dist -> String -> Dist -> [Package] -> IO ()
update branched stream =
  repoAction branched True (Header True doUpdate)
  where
    doUpdate :: Package -> IO ()
    doUpdate pkg = do
      hckg <- isFromHackage pkg
      if hckg
        then cmd_ "cabal-rpm" ["update", "-s", stream]
        else putStrLn "skipping since not hackage"

refresh :: Dist -> Bool -> Dist -> [Package] -> IO ()
refresh branched dryrun =
  repoAction branched True (Header True refreshPkg)
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

cblrpm :: Dist -> String -> Dist -> [Package] -> IO ()
cblrpm _ "" = error "CMD string must be given"
cblrpm branched cs =
  repoAction branched True (Header True doCblRpm)
  where
    doCblRpm :: Package -> IO ()
    doCblRpm p = do
    hckg <- isFromHackage p
    when hckg $
      cmd_ "cblrpm" [cs]

unbranched :: Dist -> Dist -> [Package] -> IO ()
unbranched branched dist =
  mapM_ checkBranch
  where
    checkBranch :: Package -> IO ()
    checkBranch pkg =
      withCurrentDirectory pkg $ do
      dead <- doesFileExist "dead.package"
      unless dead $ do
        let distbranch = distBranch branched dist
        branch <- gitBool "show-ref" ["--verify", "--quiet", "refs/heads/" ++ distbranch]
        unless branch $ do
          remotebranch <- gitBool "ls-remote" ["--exit-code", "--refs", "origin", distbranch]
          unless remotebranch $
            putStrLn pkg

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,2))
#else
-- | 'gitBool c args' runs git command and return result
gitBool :: String -- ^ git command
        -> [String] -- ^ arguments
        -> IO Bool -- ^ result
gitBool c args = do
  mout <- cmdMaybe "git" (c:args)
  return $ isJust mout
#endif
