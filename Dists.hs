-- |
-- Module      :  Dists
-- Copyright   :  (C) 2014-2017  Jens Petersen
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

module Dists (Dist,
              dists,
              distBranch,
              distShort,
              distVersion,
              distRepo,
              distTag,
              distTarget,
              distUpdates,
              distOverride,
              hackageRelease,
              releaseVersion,
              rawhide) where

import Data.Char (isNumber)
import Data.Maybe (fromMaybe, maybe)

import Utils ((+-+))

type Dist = String

dists :: [Dist]
dists = [rawhide, "f28", "f27", "f26", "epel7"]

rawhide :: String
rawhide = "f29"

sidetag :: Maybe String
sidetag = Nothing -- Just "ghc"

hackageRelease :: String
hackageRelease = "f28"

distBranch :: Dist -> String
distBranch d | d == rawhide = "master"
             | otherwise = d

splitDist :: Dist -> (String, String)
splitDist = break isNumber

distShort :: Dist -> String
distShort = fst . splitDist

distVersion :: Dist -> String
distVersion = snd . splitDist

distRepo :: Dist -> String
distRepo d | d == rawhide = "rawhide"
           | distShort d == "f" = "fedora"
           | distShort d `elem` ["epel", "el"] = "epel"
distRepo d = error $ "Unknown dist" +-+ d

distUpdates :: Dist -> Maybe String
distUpdates d | distShort d == "f" = Just "updates"
distUpdates _ = Nothing

distOverride :: Dist -> Bool
distOverride d = d `notElem` [rawhide, "f29" , "epel8"]

distTag :: Dist -> String
distTag d = d ++ "-" ++ fromMaybe "build" sidetag

distTarget  :: Dist -> String
distTarget d = maybe d (\ suff -> d ++ "-" ++ suff) sidetag

releaseVersion :: Dist -> String
releaseVersion r | r == rawhide = "rawhide"
releaseVersion r = distVersion r
