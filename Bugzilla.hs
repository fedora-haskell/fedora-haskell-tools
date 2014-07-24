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
import Control.Monad (unless)
import Data.Char (isLetter)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs, getProgName)
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

main :: IO ()
main = do
  dir <- getCurrentDirectory
  unless ("cblrepo/f22" `isSuffixOf` dir) $
    error $ "Not in f22/!" +-+ cblrepoHelp
  db <- doesFileExist "cblrepo.db"
  unless db $ error $
    "No cblrepo.db!" +-+ cblrepoHelp
  ghc <- shell "cblrepo list | grep '^ghc  '"
  unless ("ghc  7.6.3" `isPrefixOf` ghc) $
    error $ "cblrepo.db does not contain ghc-7.6.3:" +-+ ghc
  args <- getArgs
  bugs <- parseLines . lines <$> bugzillaQuery (["--cc=haskell-devel@lists.fedoraproject.org", "--bug_status=NEW", "--short_desc=is available", "--outputformat=%{id}\n%{component}\n%{bug_status}\n%{summary}\n%{status_whiteboard}"] ++ if null args then [] else ["--component=" ++ intercalate "," args])
  mapM_ checkBug bugs

cblrepoHelp :: String
cblrepoHelp = "Please run in haskell-sig/cblrepo/f22/ dir.\nGet it with: git clone git://git.fedorahosted.org/git/haskell-sig.git"

bugzillaQuery :: [String] -> IO String
bugzillaQuery args = cmd "bugzilla" ("query":args)

bugzillaModify :: [String] -> IO ()
bugzillaModify args = cmd_ "bugzilla" ("modify":args)

parseLines :: [String] -> [BugState]
parseLines [] = []
-- final empty whiteboard eaten by lines
parseLines (bid:bcomp:bst:bsum:[]) = [BugState bid bcomp bst bsum ""]
parseLines (bid:bcomp:bst:bsum:bwh:rest) =
  BugState bid bcomp bst bsum bwh : parseLines rest
parseLines _ = error "Bad bugzilla query output!"

checkBug :: BugState -> IO ()
checkBug (BugState bid bcomp _bst bsum bwh) =
  unless (bcomp `elem` excludedPkgs) $ do
    let pkgver = removeSuffix " is available" bsum
        hkgver = removeGhcPrefix pkgver
        hkgcver = comma hkgver
        hkg = removeGhcPrefix bcomp
    unless (null bwh || hkg `isPrefixOf` bwh) $
      putStrLn $ "Whiteboard format warning for" +-+ hkgver ++ ":" +-+ bwh +-+ "<" ++ "http://bugzilla.redhat.com/" ++ bid ++ ">"
    unless ((hkgver ++ ":") `isPrefixOf` bwh) $ do
      cblrp <- cmd "cblrepo" ["-n", "add", hkgcver]
      let state = if null cblrp then "ok" else "NG"
      putStrLn $ "*" +-+ (if null bwh then "New" else bwh +-+ "->") +-+ hkgver ++ ":" ++ state
      unless (null cblrp) $
        putStrLn cblrp
      updateBug bid bcomp hkgver cblrp state

excludedPkgs :: [String]
-- git-annex made cblrepo use 9GB of vmem...
excludedPkgs = ["ghc", "git-annex"]

removeGhcPrefix :: String -> String
removeGhcPrefix p@('g':'h':'c':'-':rest) | isLetter $ head rest = rest
                                        | otherwise = p
removeGhcPrefix pkg = pkg

comma :: String -> String
comma nv = reverse eman ++ "," ++ reverse rev
  where
    (rev, '-':eman) = break (== '-') $ reverse nv

updateBug :: String -> String -> String -> String -> String -> IO ()
updateBug bid bcomp hkgver cblrp state = do
  rebuilds <- if null cblrp then tail . lines <$> cmd "cblrepo" ["build", removeGhcPrefix bcomp] else return []
  progname <- getProgName
  let comment = progname ++ ":" +-+
                if null cblrp
                then "Rawhide can be updated to" +-+ hkgver +-+ "\naccording to the current cblrepo data in haskell-sig.git" ++ (if null rebuilds then "\nwithout any other package rebuilds." else ".\n\nIt would require rebuilding:\n" +-+  unwords rebuilds)
                else "cblrepo output for" +-+ hkgver ++ ":\n\n" ++ cblrp
  bugzillaModify ["--whiteboard==" ++ hkgver ++ ":" ++ state,
                  "--comment=" ++ comment,
                  bid]
