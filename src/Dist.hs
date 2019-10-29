module Dist
  (distArg,
   distRemote,
   distTag,
   distTarget,
   hackageRelease)
  where

import Data.Maybe (fromMaybe)
import FedoraDists (Dist(..), distBranch)
import SimpleCmdArgs (Parser, argumentWith, auto)


-- | Used for Koji sidetag when needed.
sidetag :: Dist -> Maybe String
--sidetag (Fedora n) | n == 31 = Just "ghc"
sidetag _ = Nothing

-- | Maps `Dist` to build tag
distTag :: Dist -> String
distTag d = show d ++ "-" ++ fromMaybe "build" (sidetag d)

-- | Maps `Dist` to target tag
distTarget  :: Dist -> String
distTarget d = show d ++ maybe "" ("-" ++) (sidetag d)

-- | optparse-application DIST arg
distArg :: Parser Dist
distArg = argumentWith auto "DIST"

-- | Maps `Dist` to remote branch: eg "origin/master"
distRemote :: Dist -> String
distRemote d = "origin/" ++ distBranch d

-- | Fedora release being tracked in Hackage Distro data
hackageRelease :: Dist
hackageRelease = Fedora 31
