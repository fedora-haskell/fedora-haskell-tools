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

-- Todo:
-- cache dist package lists?
-- push subpackages to Hackage
-- compare branch versions
-- compare with LTS
-- query-format string

module Main where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Data.Char (isUpper, toLower, toUpper)
import Data.Maybe
import Data.List (find, isInfixOf, isPrefixOf, nub, partition, sort, (\\))

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getHomeDirectory,
                         setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStrLn, stderr)
--import System.Posix.Env (getEnv)
import Text.CSV (parseCSV)
import Text.Read (readMaybe)

import Dists (Dist, dists, distBranch, hackageRelease, rawhide, releaseVersion)
import Koji (kojiListPkgs)
import RPM (buildRequires, haskellSrcPkgs, Package, pkgDir,
            repoquery, rpmspec)
import Utils ((+-+), checkPkgsGit, cmd, cmd_, cmdBool, cmdMaybe, cmdSilent,
              maybeRemovePrefix, removePrefix, removeSuffix,
              withCurrentDirectory)

main :: IO ()
main = do
  as <- getArgs
  if null as
    then help ""
    else
    runCommand $ parseCmdArgs as

runCommand :: Arguments -> IO ()
runCommand (com, os, ps) = do
  let (global, opts) = partition (`elem` globalOpts) os
      allpkgs = OptNull 'A' `elem` global
      mdist = getOptVal (OptArg 'b' "brnch") global
  case mdist of
    Nothing -> return ()
    Just d -> if (d `elem` dists || "rhel" `isPrefixOf` d)
      then return ()
      else putStrLn "Unknown branch"
  when (not allpkgs && null ps && com `notElem` [Hackage, CompareHackage, Count]) $
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
        CompareHackage -> do
          checkHackageDist mdist
          withPackages (Just hackageRelease) pkgs $ compareHackage (null pkgs) hackageRelease
        New -> newPackages mdist >>= mapM_ putStrLn

        Clone -> repoAction_ True False mdist global (return ()) pkgs
        CloneNew ->
          newPackages mdist >>= repoAction_ True False mdist global (return ())
        Checkout -> repoAction_ True False mdist global (return ()) pkgs
        Pull -> repoAction_ True False mdist global (cmd_ "git" ["pull", "--rebase"]) pkgs
        Push -> repoAction_ True False mdist global (cmd_ "git" ["push"]) pkgs
        Merge -> repoAction_ True False mdist global (gitMerge opts) pkgs
        Diff -> repoAction_ False False mdist global (gitDiff opts) pkgs
        DiffOrigin -> repoAction_ True False mdist global (cmd_ "git" ["--no-pager", "diff", maybe "origin" ("origin/" ++) mdist]) pkgs
        DiffBranch -> repoAction False True mdist global compareRawhide pkgs
        DiffStackage -> repoAction True True mdist global compareStackage pkgs
        Verrel -> repoAction_ False True mdist global (cmd_ "fedpkg" ["verrel"]) pkgs
        Update -> repoAction True True mdist global updatePackage pkgs
        Refresh -> repoAction_ True True mdist global (cmd_ "cabal-rpm" ["refresh"]) pkgs
        Prep -> repoAction_ True True mdist global (cmd_ "fedpkg" ["prep"]) pkgs
        Commit -> repoAction_ True True mdist global (commitChanges opts) pkgs
        Subpkgs -> repoAction True True mdist global (\ p -> rpmspec [] (Just "%{name}-%{version}") (p ++ ".spec") >>= putStrLn) pkgs
        Missing -> repoAction True True mdist global (checkForMissingDeps mdist) pkgs
        Cmd -> repoAction_ True True mdist global (execCmd opts) pkgs
  where
    checkHackageDist mdist =
      unless (isNothing mdist || mdist == Just hackageRelease) $ error $ "Hackage is currently for" +-+ hackageRelease ++ "!"

    withPackages :: Maybe Dist -> [Package] -> ([Package] -> IO ()) -> IO ()
    withPackages mdst pkgs act =
      (if null pkgs then repoqueryHaskell False mdst else return pkgs) >>= act

-- name, summary
data Command = Command { cmdName :: CmdName , cmdDescription :: String}

data CmdName = Checkout | Clone | CloneNew | Cmd | Count | Diff | DiffOrigin
             | DiffBranch | DiffStackage | Hackage | CompareHackage
             | List | Merge | New | Prep | Commit | Pull | Push | Update
             | Refresh | Subpkgs | Missing | Verrel
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
           , Command CompareHackage "compare with Hackage distro data"
           , Command List "list packages"
           , Command Merge "git merge"
           , Command Missing "missing dependency source packages"
           , Command New "new unbuilt packages"
           , Command Prep "fedpkg prep"
           , Command Commit "fedpkg commit"
           , Command Pull "git pull repos"
           , Command Push "git push repos"
           , Command Update "cabal-rpm update"
           , Command Refresh "cabal-rpm refresh"
           , Command Subpkgs "list subpackages"
           , Command Verrel "show nvr of packages"]

-- (mandatory, optional)
--type CommandOptions = ([Option], [Option])
type CommandOptions = [(Option, Bool)]

cmdOpts :: CmdName ->  CommandOptions
cmdOpts Commit = [(OptArg 'm' "\"COMMITMSG\"", False)]
cmdOpts Diff = [(OptArg 'w' "BRANCH", True)]
cmdOpts Merge = [(OptArg 'f' "BRANCH", False)]
cmdOpts Cmd = [(OptLong "cmd" "\"command\"", False)]
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

getOptVal :: Option -> [Option] -> Maybe String
getOptVal (OptNull _) _ = Nothing
getOptVal opt opts = maybe Nothing mval optval
  where
    optval = find (== opt) opts
    mval (OptNull _) = Nothing
    mval (OptArg _ v) = Just v
    mval (OptLong _ v) = Just v

isNull :: Option -> Bool
isNull (OptNull _) = True
isNull _ = False

instance Show Option
  where
    show (OptNull c) = "-" ++ [c]
    show (OptArg c v) = "-" ++ [c] ++ ('=':v)
    show (OptLong s v) = "--" ++ s ++ ('=':v)

showOpts :: [Option] -> String
showOpts = unwords . map (\ s -> "[" ++ show s ++ "]")

showCmdOpts :: CmdName -> String
showCmdOpts = unwords . map
              (\ (s,o) -> if o then "[" ++ show s ++ "]" else show s) . cmdOpts

describeOpt :: (Option, String) -> String
describeOpt (opt, desc) =
  "  " ++ show opt ++ ":" +-+ desc

type Arguments = (CmdName, [Option], [Package])

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
          in if OptNull 'A' `elem` opts && (not . null) pkgs
             then error "Can't give packages with -A"
             else (c, opts, pkgs)
  where
    isFlag ['-', c] | c /= '-' = True
    isFlag ('-':c:'=':_) | c /= '-' = True
    isFlag _ = False

    parseOpt :: String -> Option
    parseOpt [] = error "Empty option"
    parseOpt ['-',l] = OptNull l
    parseOpt ('-':l:'=':val) = OptArg l val
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
  bin <- words <$> repoquery relver ["--qf=%{source_name}", "--whatrequires", "libHSbase-*-ghc*.so()(64bit)"]
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

repoAction :: Bool -> Bool -> Maybe Dist -> [Option] -> (Package -> IO ()) -> [Package] -> IO ()
repoAction _ _ _ _ _ [] = return ()
repoAction header needsSpec mdist opts action (pkg:rest) = do
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
      cmd_ "fedpkg" $ ["clone"] ++ ["-a" | not haveSSH] ++ (if OptNull 'B' `elem` opts then ["-B"] else ["-b", branch]) ++ [pkg]
    singleDir <- doesFileExist $ pkg </> ".git/config"
    unless singleDir $ do
      branchDir <- doesDirectoryExist $ pkg </> branch
      unless branchDir $
        withCurrentDirectory pkg $
          cmd_ "fedpkg" ["clone", "-b", branch, pkg, branch]
    wd <- pkgDir pkg branch ""
    setCurrentDirectory wd
    pkggit <- do
      gd <- doesFileExist ".git/config"
      if gd
        then checkPkgsGit
        else return False
    unless pkggit $
      error $ "not a Fedora pkg git dir!:" +-+ wd
    when dirExists $ do
      actual <- gitBranch
      when (branch /= actual) $
        cmd_ "fedpkg" ["switch-branch", branch]
    isDead <- doesFileExist "dead.package"
    unless isDead $ do
      let spec = pkg ++ ".spec"
      hasSpec <- doesFileExist spec
      unless hasSpec $ putStrLn "No spec file!"
      unless (needsSpec && not hasSpec) $
        action pkg
  repoAction header needsSpec mdist opts action rest

repoAction_ :: Bool -> Bool -> Maybe Dist -> [Option] -> IO () -> [Package] -> IO ()
repoAction_ header needsSpec mdist opts action =
  repoAction header needsSpec mdist opts (const action)

gitBranch :: IO String
gitBranch =
  removePrefix "* " . head . filter (isPrefixOf "* ") . lines <$> cmd "git" ["branch"]

compareStackage :: Package -> IO ()
compareStackage p = do
  nvr <- cmd "fedpkg" ["verrel"]
  stkg <- cmdMaybe "stackage" ["package", "lts", maybeRemovePrefix "ghc-" p]
  let same = isJust stkg && fromJust stkg `isInfixOf` nvr
  putStrLn $ removePrefix (p ++ "-") nvr +-+ "(fedora)"
  putStrLn $ (if same then "same" else fromMaybe "none" stkg) +-+ "(lts)"


compareRawhide :: Package -> IO ()
compareRawhide p = do
  let spec = p ++ ".spec"
  nvr <- removeDisttag <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec
  nvr' <- withCurrentDirectory "../master" $ do
    haveSpec <- doesFileExist spec
    unless haveSpec $ cmdSilent "git" ["pull"]
    removeDisttag <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec
  if nvr == nvr'
    then putStrLn nvr
    else do
    putStrLn nvr
    putStrLn nvr'
  putStrLn ""
  where
    removeDisttag = reverse . tail . dropWhile (/= '.') . reverse

updatePackage :: Package -> IO ()
updatePackage pkg = do
  hckg <- cmdBool "grep" ["-q", "hackage.haskell.org/package/", pkg ++ ".spec"]
  if hckg
    then cmd_ "cabal-rpm" ["update"]
    else putStrLn "skipping since not hackage"

commitChanges :: [Option] -> IO ()
commitChanges [OptArg 'm' msg] = do
  chgs <- cmd "git" ["diff"]
  if null chgs
    then putStrLn "no changes"
    else cmd_ "fedpkg" ["commit", "-m", msg]
commitChanges _ = error "commit requires: -m=\"commit message\""

gitMerge :: [Option] -> IO ()
gitMerge [OptArg 'f' branch] = cmd_ "git" ["merge", branch]
gitMerge _ = error "merge needs -f=BRANCH option"

gitDiff :: [Option] -> IO ()
gitDiff [OptArg 'w' branch] = cmd_ "git" (gitDiffSubcmd ++ [branch])
gitDiff [] = cmd_ "git" gitDiffSubcmd
gitDiff _ = error "diff does not take this option"

gitDiffSubcmd :: [String]
gitDiffSubcmd = ["--no-pager", "diff"]

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
