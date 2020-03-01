module Dist
  (distArg,
   distOverride,
   distRemote,
   distTag,
   distTarget,
   hackageRelease,
   ltsStream)
  where

import Data.Maybe (fromMaybe)
import Distribution.Fedora (Dist(..), distBranch, distOverride)
import SimpleCmdArgs (Parser, argumentWith, auto)


-- | Used for Koji sidetag when needed.
sidetag :: Dist -> Maybe String
--sidetag (Fedora n) | n >= 32 = Just "build-side-19539"
sidetag _ = Nothing

-- | Maps `Dist` to build tag
distTag :: Dist -> String
--distTag (Fedora n) | n >= 32 = "f32-build-side-19539"
distTag d = show d ++ "-" ++ fromMaybe "build" (sidetag d)

-- | Maps `Dist` to target tag
distTarget  :: Dist -> String
--distTarget (Fedora n) | n >= 32 = "f32-build-side-19539"
distTarget d = show d ++ maybe "" ("-" ++) (sidetag d)

-- | optparse-application DIST arg
distArg :: Parser Dist
distArg = argumentWith auto "DIST"

-- | Maps `Dist` to remote branch: eg "origin/master"
distRemote :: Dist -> Dist -> String
distRemote branch d = "origin/" ++ distBranch branch d

-- | Fedora release being tracked in Hackage Distro data
hackageRelease :: Dist
hackageRelease = Fedora 32

-- | Stackage LTS stream major version
ltsStream :: String
ltsStream = "lts-14"
