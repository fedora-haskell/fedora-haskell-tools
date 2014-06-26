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
-- import Control.Monad (unless, when)
-- import Data.Maybe (catMaybes, fromMaybe)
-- import Data.List (find, isPrefixOf, nub, stripPrefix, union)

-- import System.Directory (doesDirectoryExist, doesFileExist)
-- import System.Environment (getArgs, getProgName)
-- import System.Exit (ExitCode (..), exitWith)
-- import System.FilePath ((</>))
-- import System.IO (hPutStrLn, stderr)

-- import Dists
import Utils

data BugState = BugState {
  bugNo :: String,
  status :: String,
  summary :: String,
  whiteboard :: String
  }

main :: IO ()
main = do
  bugs <- parseLines . lines <$> bugzilla "query" ["--cc=haskell-devel@lists.fedoraproject.org", "--bug_status=NEW", "--short_desc=is available", "--outputformat=%{id}\n%{bug_status}\n%{summary}\n%{status_whiteboard}"]
  mapM_ checkBug bugs

checkBug :: BugState -> IO ()
checkBug (BugState bid bst bsum bwh) = do
  putStrLn bsum

bugzilla :: String -> [String] -> IO String
bugzilla c args = cmd "bugzilla" (c:args)

parseLines :: [String] -> [BugState]
parseLines [] = []
-- final empty whiteboard eaten by lines
parseLines (bid:bst:bsum:[]) = [BugState bid bst bsum ""]
parseLines (bid:bst:bsum:bwh:rest) =
  BugState bid bst bsum bwh : parseLines rest
parseLines _ = error "Bad bugzilla query output!"
