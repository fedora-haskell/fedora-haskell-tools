{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Bugzilla
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: Updating of Upstream Release Monitoring bugs

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

import Control.Applicative ((<$>))
import Control.Monad (unless, when)
import Data.Char (isLetter)
import Data.List (intercalate, {-isInfixOf, isSuffixOf,-} isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory ({-doesFileExist, getCurrentDirectory,-} getModificationTime)
import System.Environment (getArgs, getEnv, getProgName)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              getOpt, usageInfo)
import System.FilePath ((</>))

-- import System.Exit (ExitCode (..), exitWith)
-- import System.FilePath ((</>))
-- import System.IO (hPutStrLn, stderr)

-- import Dists
import Utils

data BugState = BugState {
  bugNo :: String,
  component :: String,
  status :: String,
  summary :: String,
  whiteboard :: String
  }

ghcVersion :: String
ghcVersion = "7.8.3"

data Flag = Force | DryRun | NoComment | Refresh | State String
   deriving (Eq, Show)

options :: [OptDescr Flag]
options =
 [ Option "f" ["force"]  (NoArg Force)  "update even if no version change (implies --refresh)"
 , Option "n" ["dryrun"] (NoArg DryRun) "do not update bugzilla"
 , Option "r" ["refresh"] (NoArg Refresh) "update if status changed"
 , Option "s" ["state"]  (ReqArg State "BUGSTATE") "bug state (default NEW)"
 , Option "N" ["no-comment"]  (NoArg NoComment) "update the whiteboard only"
 ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
   case getOpt Permute options argv of
      (os,ps,[]  ) -> return (os,ps)
      (_,_,errs) -> do
        prog <- getProgName
        error $ concat errs ++ usageInfo (header prog) options
  where header prog = "Usage:" +-+ prog +-+ "[OPTION...] [package]..."

main :: IO ()
main = do
  (opts, args) <- getArgs >>= parseOpts
  let state = fromMaybe "NEW" $ listToMaybe $ map (\ (State s) -> s) $ filter (\ o -> case o of (State _) -> True ; _ -> False) opts
  bugs <- parseLines . lines <$> bugzillaQuery (["--cc=haskell-devel@lists.fedoraproject.org", "--bug_status=" ++ state, "--short_desc=is available", "--outputformat=%{id}\n%{component}\n%{bug_status}\n%{summary}\n%{status_whiteboard}"] ++ if null args then [] else ["--component=" ++ intercalate "," args])
  mapM_ (checkBug opts) bugs

bugzillaQuery :: [String] -> IO String
bugzillaQuery args = cmd "bugzilla" ("query":args)

bugzillaModify :: [String] -> IO ()
bugzillaModify args = cmd_ "bugzilla" ("modify":args)

parseLines :: [String] -> [BugState]
parseLines [] = []
-- final empty whiteboard eaten by lines
parseLines [bid, bcomp, bst, bsum] = [BugState bid bcomp bst bsum ""]
parseLines (bid:bcomp:bst:bsum:bwh:rest) =
  BugState bid bcomp bst bsum bwh : parseLines rest
parseLines _ = error "Bad bugzilla query output!"

checkBug :: [Flag] -> BugState -> IO ()
checkBug opts (BugState bid bcomp _bst bsum bwh) =
  unless (bcomp `elem` excludedPkgs) $ do
    let hkg = removeGhcPrefix bcomp
        (hkgver, state) = colon bwh
        hkgver' = removeGhcPrefix $ removeSuffix " is available" bsum
    unless (null hkgver || hkg `isPrefixOf` hkgver) $
      putStrLn $ "Whiteboard format warning for" +-+ hkgver' ++ ":" +-+ bwh +-+ "<" ++ "http://bugzilla.redhat.com/" ++ bid ++ ">"
    -- should not happen!
    unless (hkg `isPrefixOf` hkgver') $
      putStrLn $ "Component and Summary inconsistent!" +-+ hkg +-+ hkgver' +-+ "<" ++ "http://bugzilla.redhat.com/" ++ bid ++ ">"
    let force = Force `elem` opts
        refresh = Refresh `elem` opts
    when (hkgver /= hkgver' || force || refresh) $ do
      updateCabalPackages
      cblrp <- cmdStdErr "cblrpm" ["missingdeps", hkgver']
      let state' = if null cblrp then "ok" else "deps"
      when ((hkgver, state) /= (hkgver', state') || force) $ do
        let statemsg = if null state || state == state' then state' else state +-+ "->" +-+ state'
        putStrLn $ if hkgver == hkgver'
                   then hkgver ++ ":" +-+ statemsg
                   else (if null bwh then "New" else hkgver +-+ "->") +-+ hkgver' ++ ":" +-+ statemsg
        unless (null cblrp) $
          putStrLn cblrp
        putStrLn ""
        unless (DryRun `elem` opts) $ do
          let nocomment = NoComment `elem` opts
          updateBug bid bcomp hkgver' cblrp state' nocomment

excludedPkgs :: [String]
excludedPkgs = ["ghc", "emacs-haskell-mode"]

removeGhcPrefix :: String -> String
removeGhcPrefix p@('g':'h':'c':'-':rest) | isLetter $ head rest = rest
                                        | otherwise = p
removeGhcPrefix pkg = pkg

comma :: String -> String
comma nv = reverse eman ++ "," ++ reverse rev
  where
    (rev, '-':eman) = break (== '-') $ reverse nv

colon :: String -> (String, String)
colon "" = ("","")
colon ps = (nv, if null s then "" else removePrefix ":" s)
  where
    (nv, s) = break (== ':') ps

updateBug :: String -> String -> String -> String -> String -> Bool -> IO ()
updateBug bid _bcomp hkgver cblrp state nocomment = do
--  rebuilds <- if null cblrp then tail . lines <$> cmd "cblrepo" ["build", removeGhcPrefix bcomp] else return []
  progname <- getProgName
  let comment = progname ++ ":" +-+
                if null cblrp
                then "No missing dependencies for" +-+ hkgver +-+ "\naccording to cblrpm missingdeps" {-++ (if null rebuilds then "\nwithout any other package rebuilds." else ".\n\nIt would require also rebuilding:\n" +-+  unwords rebuilds)-}
                else  "cblrpm missingdeps output for" +-+ hkgver ++ ":\n\n" ++ cblrp
  bugzillaModify $ ["--whiteboard==" ++ hkgver ++ ":" ++ state] ++
    (if nocomment then [] else ["--comment=" ++ comment]) ++ [bid]

updateCabalPackages :: IO ()
updateCabalPackages = do
  home <- getEnv "HOME"
  pkgs <- getModificationTime (home </> ".cabal/packages/hackage.haskell.org/00-index.tar.gz")
  now <- getCurrentTime
  let diff = diffUTCTime now pkgs
  when (diff > 36000) $
    cmd_ "cabal" ["update"]
