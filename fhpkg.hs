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
-- arbitrary command
-- query-format string

module Main where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Data.Char (isUpper, toLower, toUpper)
import Data.Maybe
import Data.List (isInfixOf, isPrefixOf, nub, partition, sort, (\\))
import Data.List.NonEmpty (NonEmpty(..), fromList)

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getHomeDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStrLn, stderr)
--import System.Posix.Env (getEnv)
import Text.CSV (parseCSV)
import Text.Read (readMaybe)

import Dists (Dist, dists, distBranch, hackageRelease, releaseVersion)
import Koji (kojiListPkgs)
import RPM (repoquery, rpmspec)
import Utils ((+-+), checkFedoraPkgGit, cmd, cmd_, cmdBool, cmdMaybe, cmdSilent,
              maybeRemovePrefix, removePrefix, removeSuffix,
              withCurrentDirectory)

main :: IO ()
main = do
  as <- getArgs
  if null as
    then help ""
    else
    let (com, opts, mdist, pkgs) = parseCmdArgs (fromList as) in
    case com of
        List -> withPackages mdist pkgs (mapM_ putStrLn)
        Count -> withPackages mdist pkgs (print . length)
        Hackage -> do
          unless (isNothing mdist || mdist == Just hackageRelease) $ error $ "Hackage is currently for" +-+ hackageRelease ++ "!"
          withPackages (Just hackageRelease) pkgs $ repoqueryHackageCSV hackageRelease
        CompareHackage -> do
          unless (isNothing mdist || mdist == Just hackageRelease) $ error $ "Hackage is currently for" +-+ hackageRelease ++ "!"
          withPackages (Just hackageRelease) pkgs $ compareHackage (null pkgs) hackageRelease
        Clone -> withPackages mdist pkgs $
                   repoAction_ True False mdist opts (return ())
        CloneNew -> do
          new <- newPackages mdist
          withPackages mdist new $ repoAction_ True False mdist opts (return ())
        Pull -> withPackages mdist pkgs $
                  repoAction_ True False mdist opts (cmd_ "git" ["pull", "--rebase"])
        Push -> withPackages mdist pkgs $
                  repoAction_ True False mdist opts (cmd_ "git" ["push"])
        Diff -> withPackages mdist pkgs $
                  repoAction_ False False mdist opts (cmd_ "git" ["--no-pager", "diff"])
        DiffOrigin -> withPackages mdist pkgs $
                  repoAction_ True False mdist opts (cmd_ "git" ["--no-pager", "diff", "origin"])
        DiffBranch -> withPackages mdist pkgs $
                  repoAction False True mdist opts compareRawhide
        DiffStackage -> withPackages mdist pkgs $
                  repoAction True True mdist opts compareStackage
        Verrel -> withPackages mdist pkgs $
                    repoAction_ False True mdist opts (cmd_ "fedpkg" ["verrel"])
        Update -> withPackages mdist pkgs $
                  repoAction True True mdist opts updatePackage
        Refresh -> withPackages mdist pkgs $
                  repoAction_ True True mdist opts (cmd_ "cabal-rpm" ["refresh"])
        Prep -> withPackages mdist pkgs $
                    repoAction_ True True mdist opts (cmd_ "fedpkg" ["prep"])
        Commit -> withPackages mdist pkgs $
                    repoAction_ True True mdist opts (commitChanges opts)
        Subpkgs -> withPackages mdist pkgs $
                     repoAction True True mdist opts (\ p -> rpmspec [] (Just "%{name}-%{version}") (p ++ ".spec") >>= putStrLn)
        New -> newPackages mdist >>= mapM_ putStrLn
  where
    withPackages :: Maybe Dist -> [Package] -> ([Package] -> IO ()) -> IO ()
    withPackages mdist pkgs act =
      (if null pkgs then repoqueryHaskell False mdist else return pkgs) >>= act

-- name, summary
data Command = Cmd { cmdName :: CmdName
                   , cmdDescription :: String
                   }

data CmdName = Clone | CloneNew | Count | Diff | DiffOrigin | DiffBranch
                | DiffStackage | Hackage | CompareHackage | List | New | Prep
                | Commit | Pull | Push | Update | Refresh | Subpkgs | Verrel
  deriving (Read, Show, Eq)


-- SomeCommand -> "some-command"
showCmd :: CmdName -> String
showCmd c = render "" (show c)
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
commands = [ Cmd Clone "clone repos"
           , Cmd CloneNew "clone new packages"
           , Cmd Count  "count number of packages"
           , Cmd Diff  "git diff"
           , Cmd DiffOrigin  "git diff origin"
           , Cmd DiffBranch  "compare branch with master"
           , Cmd DiffStackage  "compare with stackage"
           , Cmd Hackage  "generate Hackage distro data"
           , Cmd CompareHackage  "compare with Hackage distro data"
           , Cmd List  "list packages"
           , Cmd New  "new unbuilt packages"
           , Cmd Prep  "fedpkg prep"
           , Cmd Commit  "fedpkg commit"
           , Cmd Pull  "git pull repos"
           , Cmd Push  "git push repos"
           , Cmd Update  "cabal-rpm update"
           , Cmd Refresh  "cabal-rpm refresh"
           , Cmd Subpkgs  "list subpackages"
           , Cmd Verrel  "show nvr of packages"]

cmdOpts :: CmdName ->  [Option]
cmdOpts Commit = [('m', Just "COMMITMSG")]
cmdOpts _ = []

globalOpts :: [Option]
globalOpts = [('B', Nothing)]

help :: String -> IO a
help err = do
  unless (null err) $
    hPutStrLn stderr err
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ showOpts globalOpts +-+ "CMD [DIST] [PKG]...\n"
    ++ "\n"
    ++ "Commands:\n"
  mapM_ (putStrLn . renderCmd) commands
  exitWith (ExitFailure 1)
  where
    cmds :: [CmdName]
    cmds = map cmdName commands
    mx = maximum $ map (length . showCmd) cmds
    renderCmd :: Command -> String
    renderCmd (Cmd c desc) =
      "  " ++ cmdopts ++ replicate (mx - length cmdopts) ' ' +-+ "-" +-+ desc
      where
        opts = cmdOpts c
        cmdopts = showCmd c ++ if null opts then "" else " " ++ showOpts opts

type Package = String

type Option = (Char, Maybe String)

showOpt :: Option -> String
showOpt (c, m) = "-" ++ [c] ++ maybe "" (\ m' -> '=':m') m

showOpts :: [Option] -> String
showOpts = unwords . map (\ s -> "[" ++ showOpt s ++ "]")

validOpt :: Option -> [Option] -> Bool
validOpt (c,m) [] = error $ "invalid option:" +-+ showOpt (c,m)
validOpt (c,m) ((c',m'):rest) | c /= c' = validOpt (c,m) rest
                              | isJust m /= isJust m' =
                                error $ "Bad option: should be" +-+ showOpt (c',m')
                              | otherwise = True

parseCmdArgs :: NonEmpty String -> (CmdName, [Option], Maybe Dist, [Package])
parseCmdArgs (('-':_) :| _) = error "global options not yet supported"
parseCmdArgs (name :| as) =
  let c = readCmd name
      (opts, mdist, args) = getOpts c
  in (c, opts, mdist, args)
  where
    getOpts :: CmdName -> ([Option], Maybe Dist, [String])
    getOpts c =
      let (os, args) = partition (\ cs -> head cs == '-') as
          opts =
            let res = map (parseOpt . removePrefix "-") os in
              if all (`validOpt` (globalOpts ++ cmdOpts c)) res then res
              else error "invalid option"
          (mdist, rest) =
            case map (removeSuffix "/") args of
              [] -> (Nothing, [])
              (d:pkgs) | d `elem` dists -> (Just d, pkgs)
                       | otherwise -> (Nothing, d:pkgs)
      in (opts, mdist, rest)

    parseOpt :: String -> Option
    parseOpt [] = error "Empty option"
    parseOpt [l] = (l, Nothing)
    parseOpt (l:'=':val) = (l, Just val)
    parseOpt ls = error $ "Cannot parse option:" +-+ ls

kojiListHaskell :: Bool -> Maybe Dist -> IO [Package]
kojiListHaskell verbose mdist = do
  when verbose $ putStrLn "Getting package list from Koji"
  libs <- filter (\ p -> "ghc-" `isPrefixOf` p && p `notElem` ["ghc-rpm-macros", "ghc-srpm-macros"]) <$> kojiListPkgs (fromMaybe "rawhide" mdist)
  when (null libs) $ error "No library packages found"
  return $ sort $ nub libs

repoqueryHackageCSV :: Dist -> [Package] -> IO ()
repoqueryHackageCSV dist pkgs = do
  let relver = releaseVersion dist
  -- Hackage csv chokes on final newline so remove it
  init . unlines . sort . map (replace "\"ghc-" "\"")  . lines <$> repoquery relver (["--disablerepo=*", "--enablerepo=fedora", "--enablerepo=updates", "--latest-limit=1", "--qf=\"%{name}\",\"%{version}\",\"https://src.fedoraproject.org/rpms/%{name}\""] ++ pkgs) >>= putStr

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
  fedora <- sort . map mungeRepo . lines <$> repoquery relver (["--disablerepo=*", "--enablerepo=fedora", "--enablerepo=updates", "--latest-limit=1", "--qf=%{name},%{version}"] ++ pkgs)
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
  -- fixme: should use repoquery instead:
  let relver = maybe "rawhide" releaseVersion mdist
  when verbose $ putStrLn "Getting packages from repoquery"
  bin <- words <$> repoquery relver ["--qf=%{source_name}", "--whatrequires", "libHSbase-*-ghc*.so()(64bit)"]
  when (null bin) $ error "No libHSbase consumers found!"
  return $ sort $ nub bin

newPackages :: Maybe Dist -> IO [Package]
newPackages mdist = do
  ps <- repoqueryHaskell True mdist
  kps <- kojiListHaskell True mdist
  return $ kps \\ ps

repoAction :: Bool -> Bool -> Maybe Dist -> [Option] -> (Package -> IO ()) -> [Package] -> IO ()
repoAction _ _ _ _ _ [] = return ()
repoAction header needsSpec mdist opts action (pkg:rest) = do
  withCurrentDirectory "." $ do
    let branchGiven = isJust mdist
        branch = maybe "master" distBranch mdist
    when header $
      putStrLn $ "\n==" +-+ pkg ++ (if branchGiven then ":" ++ branch else "") +-+ "=="
    -- muser <- getEnv "USER"
    -- let anon = "-a"
    home <- getHomeDirectory
    haveSSH <- doesFileExist $ home </> ".ssh/id_rsa"
    dirExists <- doesDirectoryExist pkg
    unless dirExists $
      cmd_ "fedpkg" $ ["clone"] ++ ["-a" | not haveSSH] ++ (if ('B',Nothing) `elem` globalOpts then ["-B"] else ["-b", branch]) ++ [pkg]
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
        then checkFedoraPkgGit
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

pkgDir :: String -> String -> FilePath -> IO FilePath
pkgDir dir branch top = do
  b <- doesDirectoryExist $ top </> dir </> branch
  return $ top </> dir </> if b then branch else ""

gitBranch :: IO String
gitBranch =
  (removePrefix "* " . head . filter (isPrefixOf "* ") . lines) <$> cmd "git" ["branch"]

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
commitChanges [('m', Just msg)] = do
  chgs <- cmd "git" ["diff"]
  if null chgs
    then putStrLn "no changes"
    else cmd_ "fedpkg" ["commit", "-m", msg]
commitChanges _ = error "commit requires: -m=\"commit message\""
