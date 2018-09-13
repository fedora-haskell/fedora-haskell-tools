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
              rawhide,
              rpmDistTag) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe, maybe)

import SimpleCmd ((+-+))

type Dist = String

dists :: [Dist]
dists = [rawhide, "f29", "f28", "f27", "epel7"]

rawhide :: String
rawhide = "f30"

sidetag :: Dist -> Maybe String
--sidetag "f30" = Just "ghc"
sidetag _ = Nothing

hackageRelease :: String
hackageRelease = "f29"

distBranch :: Dist -> String
distBranch d | d == rawhide = "master"
             | otherwise = d

splitDist :: Dist -> (String, String)
splitDist = break isDigit

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
distTag d = d ++ "-" ++ fromMaybe "build" (sidetag d)

distTarget  :: Dist -> String
distTarget d = maybe d (\ suff -> d ++ "-" ++ suff) (sidetag d)

releaseVersion :: Dist -> Maybe String
releaseVersion r | r == rawhide = Just "rawhide"
releaseVersion r = if all isDigit v then Just v else Nothing
  where
    v = distVersion r

rpmDistTag :: Dist -> String
rpmDistTag ('f':r) = ".fc" ++ r
rpmDistTag d = '.' : d
