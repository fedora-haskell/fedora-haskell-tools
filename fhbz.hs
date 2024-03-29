{-# LANGUAGE CPP, OverloadedStrings #-}

-- |
-- Copyright   :  (C) 2014-2018  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: Updating of Upstream Release Monitoring bugs

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Main where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (unless, when)
import Data.Char (isLetter)
import Data.List (dropWhileEnd, intercalate, isPrefixOf)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory ({-doesFileExist, getCurrentDirectory,-} getModificationTime)
import System.Environment (getEnv, getProgName)
import System.FilePath ((</>))

import Distribution.Fedora (Dist, getRawhideDist)
import Koji (kojicmd)
import SimpleCmd ((+-+), cmd, cmd_, cmdStdErr, removeStrictPrefix, removeSuffix)
import SimpleCmdArgs

data BugState = BugState {
  bugNo :: String,
  component :: String,
  status :: String,
  summary :: String,
  whiteboard :: String
  }

main :: IO ()
main =
  simpleCmdArgs Nothing "Fedora Haskell Bugzilla tool"
  "Updates Fedora Haskell release monitoring bugs" $
  run
  <$> switchWith 'f' "force" "update even if no version change (implies --refresh)"
  <*> switchWith 'n' "dryrun" "do not update bugzilla"
  <*> switchWith 'r' "refresh" "update if status changed"
  <*> strOptionalWith 's' "state" "STATE" "bug state (default NEW)" "NEW"
  <*> switchWith 'N' "no-comment" "update the whiteboard only"
  <*> switchWith 'c' "check" "check update for missing deps"
  <*> some (strArg "[Package...]")

run :: Bool -> Bool -> Bool -> String -> Bool -> Bool -> [String] -> IO ()
run force dryrun refresh state nocomment check pkgs = do
  bugs <- parseLines . lines <$> bugzillaQuery (["--bug_status=" ++ state, "--short_desc=is available", "--outputformat=%{id}\n%{component}\n%{bug_status}\n%{summary}\n%{status_whiteboard}"] ++ ["--component=" ++ intercalate "," pkgs])
  rawhide <- getRawhideDist
  mapM_ (checkBug rawhide force dryrun refresh nocomment check) bugs

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

checkBug :: Dist -> Bool -> Bool -> Bool -> Bool -> Bool -> BugState -> IO ()
checkBug rawhide force dryrun refresh nocomment check (BugState bid bcomp _bst bsum bwh) =
  unless (bcomp `elem` excludedPkgs) $ do
    let hkg = removeGhcPrefix bcomp
        (hkgver, state) = colon bwh
        pkgver = removeSuffix " is available" bsum
        hkgver' = removeGhcPrefix pkgver
    unless (null hkgver || hkg `isPrefixOf` hkgver) $
      putStrLn $ "Whiteboard format warning for" +-+ hkgver' ++ ":" +-+ bwh +-+ "<" ++ "http://bugzilla.redhat.com/" ++ bid ++ ">"
    -- should not happen!
    unless (hkg `isPrefixOf` hkgver') $
      putStrLn $ "Component and Summary inconsistent!" +-+ hkg +-+ hkgver' +-+ "<" ++ "http://bugzilla.redhat.com/" ++ bid ++ ">"
    if not check
      then closeBug rawhide dryrun bid bcomp pkgver
      else
      when (hkgver /= hkgver' || force || refresh) $ do
      cabalUpdate
      (missing, err) <- cmdStdErr "cblrpm" ["missingdeps", hkgver']
      let state' = if null missing && null err then "ok" else "deps"
      when ((hkgver, state) /= (hkgver', state') || force) $ do
        let statemsg = if null state || state == state' then state' else state +-+ "->" +-+ state'
        putStrLn $ if hkgver == hkgver'
                   then hkgver ++ ":" +-+ statemsg
                   else (if null bwh then "New" else hkgver +-+ "->") +-+ hkgver' ++ ":" +-+ statemsg
        unless (null missing) $
          putStrLn missing
        putStrLn ""
        unless dryrun $
          updateBug bid bcomp hkgver' missing state' nocomment

excludedPkgs :: [String]
excludedPkgs = ["ghc", "emacs-haskell-mode"]

removeGhcPrefix :: String -> String
removeGhcPrefix p@('g':'h':'c':'-':rest) | isLetter $ head rest = rest
                                        | otherwise = p
removeGhcPrefix pkg = pkg

-- comma :: String -> String
-- comma nv = reverse eman ++ "," ++ reverse rev
--   where
--     (rev, '-':eman) = break (== '-') $ reverse nv

colon :: String -> (String, String)
colon "" = ("","")
colon ps = (nv, if null s then "" else removeStrictPrefix ":" s)
  where
    (nv, s) = break (== ':') ps

closeBug :: Dist -> Bool -> String -> String -> String -> IO ()
closeBug rawhide dryrun bid bcomp pkgver = do
  latest <- cmd (kojicmd rawhide) ["latest-pkg", "rawhide", bcomp, "--quiet"]
  unless (null latest) $ do
    let nvr = (head . words) latest
    let nv = removeRelease nvr
    when (nv == pkgver) $ do
      putStrLn $ "closing" +-+ bid ++ ":" +-+ nv +-+ "in rawhide"
      unless dryrun $
        bugzillaModify ["--close=RAWHIDE", "--fixed_in=" ++ nvr, "--comment=Closed with fhbz from fedora-haskell-tools", bid]
  where
    removeRelease = init . dropWhileEnd (/= '-')

updateBug :: String -> String -> String -> String -> String -> Bool -> IO ()
updateBug bid _bcomp hkgver missing state nocomment = do
--  rebuilds <- if null missing then tail <$> cmdLines "cblrepo" ["build", removeGhcPrefix bcomp] else return []
  progname <- getProgName
  let comment = progname ++ ":" +-+
                if null missing
                then "No missing dependencies for" +-+ hkgver +-+ "\naccording to cblrpm missingdeps" {-++ (if null rebuilds then "\nwithout any other package rebuilds." else ".\n\nIt would require also rebuilding:\n" +-+  unwords rebuilds)-}
                else  "cblrpm missingdeps output for" +-+ hkgver ++ ":\n\n" ++ missing
  bugzillaModify $ ["--whiteboard==" ++ hkgver ++ ":" ++ state] ++
    ["--comment=" ++ comment | not nocomment] ++ [bid]

cabalUpdate :: IO ()
cabalUpdate = do
  home <- getEnv "HOME"
  pkgs <- getModificationTime (home </> ".cabal/packages/hackage.haskell.org/01-index.tar.gz")
  now <- getCurrentTime
  let diff = diffUTCTime now pkgs
  when (diff > 3600) $
    cmd_ "cabal" ["update"]
