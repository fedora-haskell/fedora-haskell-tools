-- |
-- Module      :  Dists
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Fedora dist metadata

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Dists (dists,
             distBranch,
             distShort,
             distVersion,
             distRepo,
             distTag,
             distTarget,
             distUpdates,
             distOverride,
             rawhide) where

import Data.Char (isNumber)
import Utils ((+-+))

dists :: [String]
dists = [rawhide, "f25", "f24", "f23", "epel7"]

rawhide :: String
rawhide = "f26"

distBranch :: String -> String
distBranch d | d == rawhide = "master"
             | otherwise = d

splitDist :: String -> (String, String)
splitDist = break isNumber

distShort :: String -> String
distShort = fst . splitDist

distVersion :: String -> String
distVersion = snd . splitDist

distRepo :: String -> String
distRepo d | d == rawhide = "rawhide"
           | distShort d == "f" = "fedora"
           | distShort d `elem` ["epel", "el"] = "epel"
distRepo d = error $ "Unknown dist" +-+ d

distUpdates :: String -> Maybe String
distUpdates d | distShort d == "f" = Just "updates"
distUpdates _ = Nothing

distOverride :: String -> Bool
distOverride d | d == rawhide = False
               | d == "f25" = False
--               | d == "epel8" = False
               | otherwise = True

distTag :: String -> String
distTag d | d == rawhide = rawhide ++ "-ghc"
             | otherwise = d ++ "-build"

distTarget  :: String -> Maybe String
distTarget d | d == rawhide = Just $ rawhide ++ "-ghc" -- Nothing
                  | otherwise = Nothing
