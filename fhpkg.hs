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

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, unless, when)
import Data.Char (isUpper, toLower, toUpper)
import Data.Maybe
import Data.List (find, isInfixOf, isPrefixOf, nub, partition, sort, (\\))

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getHomeDirectory,
#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
                         listDirectory,
#else
                         getDirectoryContents,
#endif
                         setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), takeFileName)
import System.IO (BufferMode(..), hPutStrLn, hSetBuffering, stderr,
                  stdout)
--import System.Posix.Env (getEnv)
import Text.CSV (parseCSV)
import Text.Read (readMaybe)

import FedoraDists (Dist, dists, distBranch, distRepo, distUpdates, hackageRelease,
              rawhide, releaseVersion)
import Koji (kojiListPkgs, rpkg)
import RPM (buildRequires, haskellSrcPkgs, Package, pkgDir,
            repoquery, rpmspec)
import SimpleCmd ((+-+), cmd, cmd_, cmdBool, cmdMaybe, cmdSilent, grep_,
              removePrefix, removeStrictPrefix, removeSuffix)
import SimpleCmd.Git (git, git_, gitBranch, isGitDir)
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
  args <- getArgs
  if null args
    then help ""
    else runCommand $ parseCmdArgs args

runCommand :: Arguments -> IO ()
runCommand (com, os, ps) = do
  let (global, opts) = partition (`elem` globalOpts) os
      allpkgs = OptNull 'A' `elem` global
      mdist = getOptVal (OptArg 'b' "brnch") global
  hSetBuffering stdout LineBuffering
  case mdist of
    Nothing -> return ()
    Just d -> if d `elem` dists || "rhel" `isPrefixOf` d
      then return ()
      else putStrLn "Unknown branch"
  when (not allpkgs && null ps && com `notElem` [Hackage, HackageCompare, Count]) $
    help "Please specify package(s)"
  if allpkgs && (not . null) ps
    then error "Cannot have '-A' and list of packages"
    else do
      pkgs <- if allpkgs
              then repoqueryHaskell False mdist
              else return ps
      case com of
        List -> mapM_ putStrLn pkgs
        Count -> repoqueryHaskell False mdist >>= (print . length)
        Hackage -> do
          -- add check for no pkg args
          checkHackageDist mdist
          repoqueryHackageCSV hackageRelease
        HackageCompare -> do
          checkHackageDist mdist
          withPackages (Just hackageRelease) pkgs $ compareHackage (null pkgs) hackageRelease
        New -> newPackages mdist >>= mapM_ putStrLn
        OldPackages -> do
          repopkgs <- repoqueryHaskell True mdist
          mapM_ putStrLn $ pkgs \\ repopkgs

        -- repo actions (header, needs-spec :: Bool)
        Checkout -> repoAction_ mdist global True False (return ()) pkgs
        Clone -> repoAction_ mdist global True False (return ()) pkgs
        CloneNew ->
          newPackages mdist >>= repoAction_ mdist global True False (return ())
        Commit -> repoAction_ mdist global True True (commitChanges mdist opts) pkgs
        Cmd -> repoAction_ mdist global True True (execCmd opts) pkgs
        Diff -> repoAction mdist global False False (gitDiff opts) pkgs
        DiffBranch -> repoAction mdist global False True compareRawhide pkgs
        DiffOrigin -> repoAction_ mdist global True False (git_ "diff" [maybe "origin" ("origin/" ++) mdist]) pkgs
        DiffStackage -> repoAction mdist global True True (compareStackage mdist) pkgs
        HeadOrigin -> repoAction mdist global False False (gitHeadAtOrigin mdist) pkgs
        Leaf -> repoAction mdist global (OptNull 'v' `elem` opts) True (checkLeafPkg opts) pkgs
        Merge -> repoAction_ mdist global True False (gitMerge opts) pkgs
        Missing -> repoAction mdist global True True (checkForMissingDeps mdist) pkgs
        Pull -> repoAction_ mdist global True False (git_ "pull" ["--rebase"]) pkgs
        Push -> repoAction_ mdist global True False (git_ "push" []) pkgs
        Prep -> repoAction_ mdist global True True (cmd_ (rpkg mdist) ["prep"]) pkgs
        Refresh -> repoAction mdist global True True (updateOrRefreshPackage True) pkgs
        Unpushed -> repoAction mdist global False True (gitLogOneLine mdist opts) pkgs
        Update -> repoAction mdist global True True (updateOrRefreshPackage False) pkgs
        Verrel -> repoAction_ mdist global False True (cmd_ (rpkg mdist) ["verrel"]) pkgs
        Subpkgs -> repoAction mdist global True True (\ p -> rpmspec [] (Just "%{name}-%{version}") (p ++ ".spec") >>= mapM_ putStrLn) pkgs
  where
    checkHackageDist mdist =
      unless (isNothing mdist || mdist == Just hackageRelease) $ error $ "Hackage is currently for" +-+ hackageRelease ++ "!"

    withPackages :: Maybe Dist -> [Package] -> ([Package] -> IO ()) -> IO ()
    withPackages mdst pkgs act =
      (if null pkgs then repoqueryHaskell False mdst else return pkgs) >>= act

-- name, summary
data Command = Command { cmdName :: CmdName , cmdDescription :: String}

data CmdName = Checkout | Clone | CloneNew | Cmd | Count
             | Diff | DiffOrigin | DiffBranch | DiffStackage
             | Hackage | HackageCompare | HeadOrigin
             | List | Merge | New | OldPackages | Prep | Commit | Pull | Push
             | Unpushed | Update | Refresh | Subpkgs | Missing | Leaf | Verrel
  deriving (Read, Show, Eq)

-- SomeCommand -> "some-command"
showCmd :: CmdName -> String
showCmd c = render "" (show c) +-+ showCmdOpts c
  where
    render :: String -> String -> String
    render os "" = os
    render "" (i:is) = render [toLower i] is
    render os (i:is) = render (os ++ if isUpper i then "-" ++ [toLower i] else [i]) is

-- "some-command" -> SomeCommand
readCmd :: String -> CmdName
readCmd c = fromMaybe (error $ "Unknown command " ++ c) (readMaybe $ parse "" c)
  where
    parse :: String -> String -> String
    parse os "" = os
    parse "" (i:is) = parse [toUpper i] is
    parse os ('-':is) = parse (os ++ [toUpper $ head is]) (tail is)
    parse os (i:is) = parse (os ++ [i]) is

commands :: [Command]
commands = [ Command Checkout "fedpkg switch-branch"
           , Command Clone "clone repos"
           , Command CloneNew "clone new packages"
           , Command Cmd "arbitrary command (with args)"
           , Command Count "count number of packages"
           , Command Diff "git diff"
           , Command DiffOrigin "git diff origin"
           , Command DiffBranch "compare branch with master"
           , Command DiffStackage "compare with stackage"
           , Command Hackage "generate Hackage distro data"
           , Command HackageCompare "compare with Hackage distro data"
           , Command HeadOrigin "head in sync with origin"
           , Command Leaf "list leaf packages"
           , Command List "list packages"
           , Command Merge "git merge"
           , Command Missing "missing dependency source packages"
           , Command New "new unbuilt packages"
           , Command OldPackages "packages not in repoquery"
           , Command Prep "fedpkg prep"
           , Command Commit "fedpkg commit"
           , Command Pull "git pull repos"
           , Command Push "git push repos"
           , Command Unpushed "show unpushed commits"
           , Command Update "cabal-rpm update"
           , Command Refresh "cabal-rpm refresh"
           , Command Subpkgs "list subpackages"
           , Command Verrel "show nvr of packages"]

type CommandOptions = [(Option, Bool)]

-- True: mandatory
-- False: optional
cmdOpts :: CmdName ->  CommandOptions
cmdOpts Commit = [(OptArg 'm' "\"COMMITMSG\"", True)]
cmdOpts Diff = [(OptArg 'w' "BRANCH", False),
                (OptNull 's', False),
                (OptArg 'u' "CONTEXT", False)]
cmdOpts Merge = [(OptArg 'f' "BRANCH", True)]
cmdOpts Cmd = [(OptLong "cmd" "\"command\"", True)]
cmdOpts Leaf = [(OptNull 'v', False)]
cmdOpts Unpushed = [(OptNull 's', False)]
cmdOpts _ = []

globalOptsDesc :: [(Option, String)]
globalOptsDesc =
  [ (OptNull 'A', "all Fedora Haskell packages")
  , (OptNull 'B', "clone branch dirs (fedpkg clone -B)")
  , (OptArg 'b' "BRANCH", "branch to use (fedpkg -b)")
  ]

globalOpts :: [Option]
globalOpts = map fst globalOptsDesc

help :: String -> IO a
help err = do
  if null err
    then do
    progName <- getProgName
    putStrLn $ "Usage:" +-+ progName +-+ "CMD" +-+ showOpts globalOptsNotAll +-+ "PKG..."
    putStrLn $ "      " +-+ progName +-+ "CMD -A \n"
    putStrLn "Global options:"
    mapM_ (putStrLn . describeOpt) globalOptsDesc
    putStrLn ""
    putStrLn "Commands:"
    mapM_ renderCmd commands
    else hPutStrLn stderr err
  exitWith (ExitFailure 1)
  where
    globalOptsNotAll = filter (/= OptNull 'A') globalOpts
    cmds :: [CmdName]
    cmds = map cmdName commands
    mx = maximum $ map (length . showCmd) cmds
    renderCmd :: Command -> IO ()
    renderCmd (Command c desc) = do
      let txt = showCmd c
      putStrLn $ "  " ++ txt ++ replicate (mx - length txt) ' ' +-+ "-" +-+ desc

-- add description
data Option = OptNull Char | OptArg Char String | OptLong String String

instance Eq Option
  where
    OptNull c == OptNull c' = c == c'
    OptArg c _ == OptArg c' _ = c == c'
    OptLong s _ == OptLong s' _ = s == s'
    _ == _ = False

hasOptNull :: Char -> [Option] -> Bool
hasOptNull c opts = OptNull c `elem` opts

getOptVal :: Option -> [Option] -> Maybe String
getOptVal (OptNull _) _ = Nothing
getOptVal opt opts = maybe Nothing mval optval
  where
    optval = find (== opt) opts
    mval (OptNull _) = Nothing
    mval (OptArg _ v) = Just v
    mval (OptLong _ v) = Just v

--isNull :: Option -> Bool
--isNull (OptNull _) = True
--isNull _ = False

instance Show Option
  where
    show (OptNull c) = "-" ++ [c]
    show (OptArg c v) = "-" ++ [c] ++ v
    show (OptLong s v) = "--" ++ s ++ ('=':v)

showOpts :: [Option] -> String
showOpts = unwords . map (\ s -> "[" ++ show s ++ "]")

showCmdOpts :: CmdName -> String
showCmdOpts = unwords . map
              (\ (s,o) -> if o then show s else "[" ++ show s ++ "]") . cmdOpts

describeOpt :: (Option, String) -> String
describeOpt (opt, desc) =
  "  " ++ show opt ++ ":" +-+ desc

type Arguments = (CmdName, [Option], [Package])

-- FIXME: should check mandatory opts present
parseCmdArgs :: [String] -> Arguments
parseCmdArgs [] = error "Need to pass more arguments" -- should not happen
parseCmdArgs as =
      let (os, args) = partition isFlag as in
        if null args
        then error "Please give an command"
        else
          let (cstr:rest) = args
              c = readCmd cstr
              opts =
                let res = map parseOpt os in
                  if all (`elem` globalOpts ++ map fst (cmdOpts c)) res then res
                  else error "invalid option"
              pkgs = map (removeSuffix "/") rest
          in if hasOptNull 'A' opts && (not . null) pkgs
             then error "Can't give packages with -A"
             else (c, opts, pkgs)
  where
    isFlag ['-', c] | c /= '-' = True
    isFlag ('-':c:_) | c /= '-' = True
    isFlag ('-':'-':_) = True
    isFlag _ = False

    parseOpt :: String -> Option
    parseOpt [] = error "Empty option"
    parseOpt ['-',l] = OptNull l
    parseOpt ('-':l:val) | l /= '-' = OptArg l val
    parseOpt ('-':'-':assgn)
      | '=' `elem` assgn =
          let (n,v) = break (== '=') assgn
          in if null n || null v then error "malformed long option"
             else OptLong n (tail v)
    parseOpt ls = error $ "Cannot parse option:" +-+ ls

repoqueryHackageCSV :: Dist -> IO ()
repoqueryHackageCSV dist = do
  pkgs <- repoqueryHaskell False (Just dist)
  let relver = releaseVersion dist
  -- Hackage csv chokes on final newline so remove it
  init . unlines . sort . map (replace "\"ghc-" "\"")  . lines <$> repoquery relver (["--repo=fedora", "--repo=updates", "--latest-limit=1", "--qf=\"%{name}\",\"%{version}\",\"https://src.fedoraproject.org/rpms/%{name}\""] ++ pkgs) >>= putStr

data PkgVer = PV { pvPkg :: String, pvVer :: String}
  deriving (Eq)

instance Show PkgVer
  where
    show (PV p v) = p ++ "-" ++ v

instance Ord PkgVer
  where
    compare (PV p _) (PV p' _) = compare p p'

compareHackage :: Bool -> Dist -> [Package] -> IO ()
compareHackage all' dist pkgs = do
  hck <- simpleHTTP (getRequest "http://hackage.haskell.org/distro/Fedora/packages.csv") >>= getResponseBody
  let hackage = sort . either (error "Malformed Hackage csv") (map mungeHackage) $ parseCSV "packages.csv" hck
      relver = releaseVersion dist
  fedora <- sort . map mungeRepo . lines <$> repoquery relver (["--repo=fedora", "--repo=updates", "--latest-limit=1", "--qf=%{name},%{version}"] ++ pkgs)
  compareSets all' hackage fedora
  where
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

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b s@(x:xs) =
  if a `isPrefixOf` s
  then b ++ replace a b (drop (length a) s)
  else x:replace a b xs
replace _ _ [] = []

repoqueryHaskell :: Bool -> Maybe Dist -> IO [Package]
repoqueryHaskell verbose mdist = do
  let relver = maybe Nothing releaseVersion mdist
  when verbose $ putStrLn "Getting packages from repoquery"
  let repo = maybe "rawhide" distRepo mdist
      updates = maybeToList $ maybe Nothing distUpdates mdist
  bin <- words <$> repoquery relver (["--repo=" ++ repo] ++ ["--repo=" ++ u | u <- updates] ++ ["--qf=%{source_name}", "--whatrequires", "libHSbase-*-ghc*.so()(64bit)"])
  when (null bin) $ error "No libHSbase consumers found!"
  return $ sort $ nub bin

newPackages :: Maybe Dist -> IO [Package]
newPackages mdist = do
  ps <- repoqueryHaskell True mdist
  kps <- kojiListHaskell True mdist
  return $ kps \\ ps

kojiListHaskell :: Bool -> Maybe Dist -> IO [Package]
kojiListHaskell verbose mdist = do
  when verbose $ putStrLn "Getting package list from Koji"
  libs <- filter (\ p -> "ghc-" `isPrefixOf` p && p `notElem` ["ghc-rpm-macros", "ghc-srpm-macros"]) <$> kojiListPkgs (fromMaybe "rawhide" mdist)
  when (null libs) $ error "No library packages found"
  return $ sort $ nub libs

repoAction :: Maybe Dist -> [Option] -> Bool -> Bool -> (Package -> IO ()) -> [Package] -> IO ()
repoAction _ _ _ _ _ [] = return ()
repoAction mdist opts header needsSpec action (pkg:rest) = do
  withCurrentDirectory "." $ do
    let branchGiven = isJust mdist
        branch = maybe "master" distBranch mdist
    when header $
      putStrLn $ "\n==" +-+ pkg ++ (if branchGiven then ":" ++ branch else "") +-+ "=="
    -- muser <- getEnv "USER"
    home <- getHomeDirectory
    haveSSH <- doesFileExist $ home </> ".ssh/id_rsa"
    dirExists <- doesDirectoryExist pkg
    unless dirExists $
      cmd_ (rpkg mdist) $ ["clone"] ++ ["-a" | not haveSSH] ++ (if hasOptNull 'B' opts then ["-B"] else ["-b", branch]) ++ [pkg]
    singleDir <- isGitDir pkg
    unless singleDir $ do
      branchDir <- doesDirectoryExist $ pkg </> branch
      unless branchDir $
        withCurrentDirectory pkg $
          cmd_ (rpkg mdist) ["clone", "-b", branch, pkg, branch]
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
        cmd_ (rpkg mdist) ["switch-branch", branch]
    isDead <- doesFileExist "dead.package"
    unless isDead $ do
      let spec = pkg ++ ".spec"
      hasSpec <- doesFileExist spec
      -- FIXME: silence for cmds that only output package names (eg unpushed -s)
      unless hasSpec $ putStrLn $ (if header then "" else pkg ++ ": ") ++ "No spec file!"
      unless (needsSpec && not hasSpec) $
        action pkg
  repoAction mdist opts header needsSpec action rest

repoAction_ :: Maybe Dist -> [Option] -> Bool -> Bool -> IO () -> [Package] -> IO ()
repoAction_ mdist opts header needsSpec action =
  repoAction mdist opts header needsSpec (const action)

compareStackage :: Maybe Dist -> Package -> IO ()
compareStackage mdist p = do
  nvr <- cmd (rpkg mdist) ["verrel"]
  let stream = "lts-11"
  stkg <- cmdMaybe "stackage" ["package", stream, removePrefix "ghc-" p]
  let same = isJust stkg && fromJust stkg `isInfixOf` nvr
  putStrLn $ removeStrictPrefix (p ++ "-") nvr +-+ "(fedora)"
  putStrLn $ (if same then "same" else fromMaybe "none" stkg) +-+ "(" ++ stream ++ ")"


compareRawhide :: Package -> IO ()
compareRawhide p = do
  let spec = p ++ ".spec"
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
  grep_ "hackage.haskell.org/package/" $ pkg ++ ".spec"


updateOrRefreshPackage :: Bool -> Package -> IO ()
updateOrRefreshPackage refresh pkg = do
  hckg <- isFromHackage pkg
  let mode = if refresh then "refresh" else "update"
  if hckg
    then cmd_ "cabal-rpm" [mode]
    else putStrLn "skipping since not hackage"

commitChanges :: Maybe Dist -> [Option] -> IO ()
commitChanges mdist [OptArg 'm' msg] = do
  chgs <- cmd "git" ["diff"]
  if null chgs
    then putStrLn "no changes"
    else cmd_ (rpkg mdist) ["commit", "-m", msg]
commitChanges _ _ = error "commit requires: -m=\"commit message\""

gitMerge :: [Option] -> IO ()
gitMerge [OptArg 'f' branch] = git_ "merge" [branch]
gitMerge _ = error "merge needs -f=BRANCH option"

gitDiff :: [Option] -> Package -> IO ()
gitDiff opts pkg = do
  let mbrnch = getOptVal (OptArg 'w' "branch") opts
      branch = maybeToList mbrnch
      short  = hasOptNull 's' opts
      mcontxt = getOptVal (OptArg 'u' "context") opts
      contxt = maybe [] (\ n -> ["-U" ++ n]) mcontxt
  out <- git "diff" $ branch ++ contxt
  if short
    then unless (null out) $ putStrLn pkg
    else putStrLn out

execCmd :: [Option] -> IO ()
execCmd [OptLong "cmd" cs]
  | null cs = error "a string must be passed to --cmd="
  | otherwise = let (c:args) = words cs in
                  cmd_ c args
execCmd _ = error "cmd needs --cmd= option"

checkForMissingDeps :: Maybe Dist -> Package -> IO ()
checkForMissingDeps mdist pkg = do
  dir <- takeFileName <$> getCurrentDirectory
  let top = if dir == pkg then ".." else "../.."
      dist = fromMaybe rawhide mdist
      spec = pkg ++ ".spec"
  hasSpec <- doesFileExist spec
  if hasSpec
    then do
    deps <- buildRequires (pkg ++ ".spec") >>= haskellSrcPkgs top dist
    mapM_ (checkMissing top) deps
    else putStrLn "no spec file found!"
  where
    checkMissing :: FilePath -> Package -> IO ()
    checkMissing top dep = do
      exists <- doesDirectoryExist $ top </> dep
      unless exists $ putStrLn $ "Missing" +-+ dep

-- fixme: make a dependency cache
checkLeafPkg :: [Option] -> Package -> IO ()
checkLeafPkg opts pkg = do
  dir <- takeFileName <$> getCurrentDirectory
  let branchdir = dir /= pkg
      top = if branchdir then "../.." else ".."
      spec = pkg ++ ".spec"
  subpkgs <- rpmspec ["--builtrpms"] (Just "%{name}") spec
  allpkgs <- listDirectory top
  let other = map (\ p -> top </> p </> (if branchdir then dir else "") </> p ++ ".spec") $ allpkgs \\ [pkg]
      verb = hasOptNull 'v' opts
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

gitLogOneLine :: Maybe Dist -> [Option] -> Package -> IO ()
gitLogOneLine mdist opts pkg = do
  out <- git "log" [maybe "origin" ("origin/" ++) mdist ++ "..HEAD", "--pretty=oneline"]
  let short = hasOptNull 's' opts
  unless (null out) $
    putStrLn $ pkg ++ if short then "" else (unwords . map replaceHash . words) out
  where
    replaceHash h = if length h /= 40 then h else ":"

gitHeadAtOrigin :: Maybe Dist -> Package -> IO ()
gitHeadAtOrigin mdist pkg = do
  same <- cmdBool "git" ["diff", "--quiet", maybe "origin" ("origin/" ++) mdist ++ "..HEAD"]
  when same $ putStrLn pkg
