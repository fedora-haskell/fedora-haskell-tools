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
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import System.Directory (doesFileExist, getCurrentDirectory)
-- import System.Environment (getArgs, getProgName)
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
  unless ("cblrepo/f21" `isSuffixOf` dir) $
    error $ "Not in f21/!" +-+ cblrepoHelp
  db <- doesFileExist "cblrepo.db"
  unless db $ error $
    "No cblrepo.db!" +-+ cblrepoHelp
  ghc <- shell "cblrepo list | grep '^ghc  '"
  unless ("ghc  7.6.3" `isPrefixOf` ghc) $
    error $ "cblrepo.db does not contain ghc-7.6.3:" +-+ ghc
  -- FIXME check component too?
  bugs <- parseLines . lines <$> bugzilla "query" ["--cc=haskell-devel@lists.fedoraproject.org", "--bug_status=NEW", "--short_desc=is available", "--outputformat=%{id}\n%{component}\n%{bug_status}\n%{summary}\n%{status_whiteboard}"]
  mapM_ checkBug bugs

cblrepoHelp :: String
cblrepoHelp = "Please run in haskell-sig/cblrepo/f21/ dir.\nGet it with: git clone git://git.fedorahosted.org/git/haskell-sig.git"

bugzilla :: String -> [String] -> IO String
bugzilla c args = cmd "bugzilla" (c:args)

parseLines :: [String] -> [BugState]
parseLines [] = []
-- final empty whiteboard eaten by lines
parseLines (bid:bcomp:bst:bsum:[]) = [BugState bid bcomp bst bsum ""]
parseLines (bid:bcomp:bst:bsum:bwh:rest) =
  BugState bid bcomp bst bsum bwh : parseLines rest
parseLines _ = error "Bad bugzilla query output!"

checkBug :: BugState -> IO ()
checkBug (BugState _bid bcomp _bst bsum bwh) =
  unless (bcomp `elem` excludedPkgs) $ do
  let pkgver = removeSuffix " is available" bsum
      hkgver = removeGhcPrefix pkgver
      hkgcver = comma hkgver
  unless ((hkgver ++ ":") `isPrefixOf` bwh ||
          (" " ++ hkgver ++ ":") `isInfixOf` bwh) $ do
    putStrLn $ "*" +-+ (if null bwh then "NEW" else bwh +-+ "->") +-+ hkgver
    cmd_ "cblrepo" ["-n", "add", hkgcver]

excludedPkgs :: [String]
excludedPkgs = ["ghc"]

removeGhcPrefix :: String -> String
removeGhcPrefix p@('g':'h':'c':'-':rest) | isLetter $ head rest = rest
                                        | otherwise = p
removeGhcPrefix pkg = pkg

comma :: String -> String
comma nv = reverse eman ++ "," ++ reverse rev
  where
    (rev, '-':eman) = break (== '-') $ reverse nv
