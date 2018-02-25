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
import Control.Arrow ((&&&))
import Control.Monad (unless, when)
import Data.Maybe
import Data.List (isInfixOf, isPrefixOf, nub, partition, sort, (\\))

import System.Directory (doesDirectoryExist, doesFileExist,
                         getHomeDirectory, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
--import System.Posix.Env (getEnv)

import Dists (Dist, dists, distBranch, hackageRelease, releaseVersion)
import Koji (kojiListPkgs)
import RPM (repoquery, rpmspec)
import Utils ((+-+), checkFedoraPkgGit, cmd, cmd_, cmdBool, cmdMaybe, cmdSilent,
              maybeRemovePrefix, removePrefix, removeSuffix,
              withCurrentDirectory)

main :: IO ()
main = do
  margs <- getArgs >>= parseArgs
  case margs of
    Nothing -> return ()
    Just (com, opts, mdist, pkgs) ->
      case com of
        "list" -> withPackages mdist pkgs (mapM_ putStrLn)
        "count" -> withPackages mdist pkgs (print . length)
        "hackage" -> do
          let currentHackage = Just hackageRelease
          unless (isNothing mdist || mdist == currentHackage) $ error $ "Hackage is currently for" +-+ fromJust currentHackage ++ "!"
          withPackages currentHackage pkgs (repoqueryHackageCSV currentHackage)
        "clone" -> withPackages mdist pkgs $
                   repoAction_ True False mdist opts (return ())
        "clone-new" -> do
          new <- newPackages mdist
          withPackages mdist new $ repoAction_ True False mdist opts (return ())
        "pull" -> withPackages mdist pkgs $
                  repoAction_ True False mdist opts (cmd_ "git" ["pull", "--rebase"])
        "push" -> withPackages mdist pkgs $
                  repoAction_ True False mdist opts (cmd_ "git" ["push"])
        "diff" -> withPackages mdist pkgs $
                  repoAction_ False False mdist opts (cmd_ "git" ["--no-pager", "diff"])
        "diff-origin" -> withPackages mdist pkgs $
                  repoAction_ True False mdist opts (cmd_ "git" ["--no-pager", "diff", "origin"])
        "diff-branch" -> withPackages mdist pkgs $
                  repoAction False True mdist opts compareRawhide
        "diff-stackage" -> withPackages mdist pkgs $
                  repoAction True True mdist opts compareStackage
        "verrel" -> withPackages mdist pkgs $
                    repoAction_ False True mdist opts (cmd_ "fedpkg" ["verrel"])
        "update" -> withPackages mdist pkgs $
                  repoAction True True mdist opts updatePackage
        "refresh" -> withPackages mdist pkgs $
                  repoAction_ True True mdist opts (cmd_ "cabal-rpm" ["refresh"])
        "prep" -> withPackages mdist pkgs $
                    repoAction_ True True mdist opts (cmd_ "fedpkg" ["prep"])
        "commit" -> withPackages mdist pkgs $
                    -- need to handle passing commit message
                    repoAction_ True True mdist opts (commitChanges "")
        "subpkgs" -> withPackages mdist pkgs $
                     repoAction True True mdist opts (\ p -> rpmspec [] (Just "%{name}-%{version}") (p ++ ".spec") >>= putStrLn)
        "new" -> newPackages mdist >>= mapM_ putStrLn
        _ -> return ()
  where
    withPackages :: Maybe Dist -> [Package] -> ([Package] -> IO ()) -> IO ()
    withPackages mdist pkgs act =
      (if null pkgs then repoqueryHaskell False mdist else return pkgs) >>= act

-- name, summary, options
data Command = Cmd { cmdName :: String
                   , cmdOptions :: [Option]
                   , cmdDescription :: String
                   }

commands :: [Command]
commands = [ Cmd "clone" ['B'] "clone repos"
           , Cmd "clone-new" ['B'] "clone new packages"
           , Cmd "count" [] "count number of packages"
           , Cmd "diff" [] "git diff"
           , Cmd "diff-origin" [] "git diff origin"
           , Cmd "diff-branch" [] "compare branch with master"
           , Cmd "diff-stackage" [] "compare with stackage"
           , Cmd "hackage" [] "generate Hackage distro data"
           , Cmd "list" [] "list packages"
           , Cmd "new" [] "new unbuilt packages"
           , Cmd "prep" [] "fedpkg prep"
           , Cmd "commit" [] "fedpkg commit"
           , Cmd "pull" [] "git pull repos"
           , Cmd "push" [] "git push repos"
           , Cmd "update" [] "cabal-rpm update"
           , Cmd "refresh" [] "cabal-rpm refresh"
           , Cmd "subpkgs" [] "list subpackages"
           , Cmd "verrel" [] "show nvr of packages"]

cmdOpts :: [(String, [Option])]
cmdOpts = map (cmdName &&& cmdOptions) commands

cmds :: [String]
cmds = map fst cmdOpts

help :: IO ()
help = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage:" +-+ progName +-+ "CMD [DIST]\n"
    ++ "\n"
    ++ "Commands:\n"
  mapM_ (putStrLn . renderCmd) commands
  exitWith (ExitFailure 1)
  where
    mx = maximum $ map length cmds
    renderCmd :: Command -> String
    renderCmd (Cmd c opts desc) =
      "  " ++ cmdopts ++ replicate (mx - length cmdopts) ' ' +-+ "-" +-+ desc
      where
        cmdopts = c ++ if null opts then "" else " [-" ++ opts ++ "]"

type Package = String

type Arguments = Maybe (String, [Option], Maybe Dist, [Package])

type Option = Char

getOpts :: [String] -> ([Option], [String])
getOpts as =
  let (optss, args) = partition (\ cs -> head cs == '-') as in
  (concatMap (removePrefix "-") optss, map (removeSuffix "/") args)

parseArgs :: [String] -> IO Arguments
parseArgs as =
  let (opts, args) = getOpts as in
    case args of
      [] -> help >> return Nothing
      (c:_) | any (`notElem` optCmd c) opts -> giveUp $ "Unknown option '-" ++ opts ++ "' for command '" ++ c ++ "'"
      [c] -> return (Just (c, opts, Nothing, []))
      (c:dist:pkgs) | dist `notElem` dists ->
                        return $ Just (c, opts, Nothing, dist:pkgs)
                    | otherwise ->
                        return $ Just (c, opts, Just dist, pkgs)
  where
    giveUp :: String -> IO Arguments
    giveUp err = do
      hPutStrLn stderr err
      help >> return Nothing

    optCmd :: String -> [Option]
    optCmd c = fromMaybe (error $ "No such command '" ++ c ++ "'") $
               lookup c cmdOpts

kojiListHaskell :: Bool -> Maybe Dist -> IO [Package]
kojiListHaskell verbose mdist = do
  when verbose $ putStrLn "Getting package list from Koji"
  libs <- filter (\ p -> "ghc-" `isPrefixOf` p && p `notElem` ["ghc-rpm-macros", "ghc-srpm-macros"]) <$> kojiListPkgs (fromMaybe "rawhide" mdist)
  when (null libs) $ error "No library packages found"
  return $ sort $ nub libs

repoqueryHackageCSV :: Maybe Dist -> [Package] -> IO ()
repoqueryHackageCSV mdist pkgs = do
  let relver = maybe "rawhide" releaseVersion mdist
  -- Hackage csv chokes on final newline so remove it
  init . unlines . sort . map (replace "\"ghc-" "\"")  . lines <$> repoquery relver (["--disablerepo=*", "--enablerepo=fedora", "--enablerepo=updates", "--latest-limit=1", "--qf=\"%{name}\",\"%{version}\",\"https://src.fedoraproject.org/rpms/%{name}\""] ++ pkgs) >>= putStr

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
      cmd_ "fedpkg" $ ["clone"] ++ ["-a" | not haveSSH] ++ (if 'B' `elem` opts then ["-B"] else ["-b", branch]) ++ [pkg]
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

commitChanges :: String -> IO ()
commitChanges msg = do
  chgs <- cmd "git" ["diff"]
  if null chgs
    then putStrLn "no changes"
    else cmd_ "fedpkg" ["commit", "-m", msg]
