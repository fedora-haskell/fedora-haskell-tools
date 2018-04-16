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
import Control.Arrow (second)
import Control.Monad (unless, when)
import Data.Char (isUpper, toLower, toUpper)
import Data.Maybe
import Data.List (intercalate, isInfixOf, isPrefixOf, nub, partition, sort, (\\))
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
    runCommand $ parseCmdArgs (fromList as)

runCommand :: Arguments -> IO ()
runCommand (com, opts, mdist, args, ps) = do
  print (com, opts, mdist, args, ps)
  let allpkgs = OptNull 'A' `elem` opts
  if allpkgs && (not . null) ps
    then error "Cannot have '-A' and list of packages"
    else do
      pkgs <- if allpkgs
              then repoqueryHaskell False mdist
              else return ps
      case com of
        List -> mapM_ putStrLn pkgs
        Count -> (print . length) pkgs
        Hackage -> do
          checkHackageDist
          repoqueryHackageCSV hackageRelease
        CompareHackage -> do
          checkHackageDist
          withPackages (Just hackageRelease) pkgs $ compareHackage (null pkgs) hackageRelease
        Clone -> withPackages mdist pkgs $
                   repoAction_ True False mdist opts (return ())
        CloneNew -> do
          new <- newPackages mdist
          withPackages mdist new $ repoAction_ True False mdist opts (return ())
        Checkout -> withPackages mdist pkgs $
                    repoAction_ True False mdist opts (return ())
        Pull -> withPackages mdist pkgs $
                  repoAction_ True False mdist opts (cmd_ "git" ["pull", "--rebase"])
        Push -> withPackages mdist pkgs $
                  repoAction_ True False mdist opts (cmd_ "git" ["push"])
        Merge -> withPackages mdist pkgs $
                  repoAction_ True False mdist opts (cmd_ "git" ("merge" : args))
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
        Cmd -> withPackages mdist pkgs $
               if null args
               then error "cmd requires args"
               else repoAction_ True True mdist opts (cmd_ (head args) (tail args))
        New -> newPackages mdist >>= mapM_ putStrLn
  where
    checkHackageDist =
      unless (isNothing mdist || mdist == Just hackageRelease) $ error $ "Hackage is currently for" +-+ hackageRelease ++ "!"

withPackages :: Maybe Dist -> [Package] -> ([Package] -> IO ()) -> IO ()
withPackages mdst ps act =
  (if null ps then repoqueryHaskell False mdst else return ps) >>= act

-- name, summary
data Command = Command { cmdName :: CmdName , cmdDescription :: String}

data CmdName = Checkout | Clone | CloneNew | Cmd | Count | Diff | DiffOrigin
             | DiffBranch | DiffStackage | Hackage | CompareHackage
             | List | Merge | New | Prep | Commit | Pull | Push | Update
             | Refresh | Subpkgs | Verrel
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
           , Command New "new unbuilt packages"
           , Command Prep "fedpkg prep"
           , Command Commit "fedpkg commit"
           , Command Pull "git pull repos"
           , Command Push "git push repos"
           , Command Update "cabal-rpm update"
           , Command Refresh "cabal-rpm refresh"
           , Command Subpkgs "list subpackages"
           , Command Verrel "show nvr of packages"]

cmdOpts :: CmdName ->  [Option]
cmdOpts Commit = [OptArg 'm' "COMMITMSG"]
cmdOpts _ = []

globalOptsDesc :: [(Option, String)]
globalOptsDesc =
  [ (OptNull 'B', "clone branch dirs (fedpkg clone -B)")
  , (OptNull 'A', "all Fedora Haskell packages")
  ]

globalOpts :: [Option]
globalOpts = map fst globalOptsDesc

help :: String -> IO a
help err = do
  unless (null err) $
    hPutStrLn stderr err
  progName <- getProgName
  putStrLn $ "Usage:" +-+ progName +-+ "CMD" +-+ showOpts globalOptsNotAll +-+ "[ARGS... --] [DIST] [PKG]..."
  putStrLn $ "      " +-+ progName +-+ "CMD -A [ARGS] [DIST]\n"
  putStrLn "Options:"
  mapM_ (putStrLn . describeOpt) globalOptsDesc
  putStrLn ""
  putStrLn "Commands:"
  mapM_ (putStrLn . renderCmd) commands
  exitWith (ExitFailure 1)
  where
    globalOptsNotAll = filter (/= OptNull 'A') globalOpts
    cmds :: [CmdName]
    cmds = map cmdName commands
    mx = maximum $ map (length . showCmd) cmds
    renderCmd :: Command -> String
    renderCmd (Command c desc) =
      "  " ++ cmdHelp
      where
        opts = cmdOpts c
        cmdHelp =
          let (nullopts, argopts) = partition isNull opts
              txt = showCmd c ++ if null opts then "" else " " ++ showOpts nullopts
              indent = "\n    "
          in
            txt ++ replicate (mx - length txt) ' ' +-+ "-" +-+ desc ++
            if null argopts then "" else indent ++ intercalate indent (map show argopts)

type Package = String

-- add description
data Option = OptNull Char | OptArg Char String

instance Eq Option
  where
    OptNull c == OptNull c' = c == c'
    OptArg c _ == OptArg c' _ = c == c'
    OptNull _ == OptArg _ _ = False
    OptArg _ _== OptNull _  = False

isNull :: Option -> Bool
isNull (OptNull _) = True
isNull (OptArg _ _) = False

instance Show Option
  where
    show (OptNull c) = "-" ++ [c]
    show (OptArg c v) = "-" ++ [c] ++ ('=':v)

showOpts :: [Option] -> String
showOpts = unwords . map (\ s -> "[" ++ show s ++ "]")

validOpt :: Option -> [Option] -> Bool
validOpt opt [] = error $ "invalid option:" +-+ show opt
validOpt opt (opt':rest) | opt /= opt' = validOpt opt rest
                         | otherwise = True

describeOpt :: (Option, String) -> String
describeOpt (opt, desc) =
  "  " ++ show opt ++ ":" +-+ desc

type Arguments = (CmdName, [Option], Maybe Dist, [String], [Package])

parseCmdArgs :: NonEmpty String -> Arguments
parseCmdArgs (('-':_) :| _) = error "options should come after command"
parseCmdArgs (name :| as) =
  let c = readCmd name
      (opts, mdist, args, pkgs) = getOpts c
  in (c, opts, mdist, args, pkgs)
  where
    getOpts :: CmdName -> ([Option], Maybe Dist, [String], [Package])
    getOpts c =
      let (os, args) = partition isFlag as
          opts =
            let res = map parseOpt os in
              if all (`validOpt` (globalOpts ++ cmdOpts c)) res then res
              else error "invalid option"
          (cargs, pkgs')
            | OptNull 'A' `elem` opts =
              if "--" `elem` args
              then error "Can't give packages with -A"
              else (args, [])
            | "--" `elem` args =
              second tail $ span (/= "--") args
            | otherwise = ([], args)
          (mdist, pkgs) =
            case map (removeSuffix "/") pkgs' of
              [] -> (Nothing, [])
              (d:ps) | d `elem` dists -> (Just d, ps)
                       | otherwise -> (Nothing, d:ps)
      in (opts, mdist, cargs, pkgs)

    isFlag ['-', c] | c /= '-' = True
    isFlag ('-':c:'=':_) | c /= '-' = True
    isFlag _ = False

    parseOpt :: String -> Option
    parseOpt [] = error "Empty option"
    parseOpt ['-',l] = OptNull l
    parseOpt ('-':l:'=':val) = OptArg l val
    parseOpt ls = error $ "Cannot parse option:" +-+ ls

repoqueryHackageCSV :: Dist -> IO ()
repoqueryHackageCSV dist = do
  pkgs <- repoqueryHaskell False (Just dist)
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
commitChanges [OptArg 'm' msg] = do
  chgs <- cmd "git" ["diff"]
  if null chgs
    then putStrLn "no changes"
    else cmd_ "fedpkg" ["commit", "-m", msg]
commitChanges _ = error "commit requires: -m=\"commit message\""
