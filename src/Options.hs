module Options
  (distArg)
  where

import FedoraDists
import Options.Applicative

distArg :: Parser Dist
distArg = argument auto (metavar "DIST")

