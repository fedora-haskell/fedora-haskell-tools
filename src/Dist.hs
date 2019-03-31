module Dist
  (distArg,
   distTag,
   distTarget)
  where

import Data.Maybe (fromMaybe)
import FedoraDists (Dist(..))
import Options.Applicative


-- | Used for Koji sidetag when needed.
sidetag :: Dist -> Maybe String
-- sidetag (Fedora n) | n == 31 = Just "ghc"
sidetag _ = Nothing

-- | Maps `Dist` to build tag
distTag :: Dist -> String
distTag d = show d ++ "-" ++ fromMaybe "build" (sidetag d)

-- | Maps `Dist` to target tag
distTarget  :: Dist -> String
distTarget d = show d ++ "-" ++ fromMaybe "" (sidetag d)

-- | optparse-application DIST arg
distArg :: Parser Dist
distArg = argument auto (metavar "DIST")

