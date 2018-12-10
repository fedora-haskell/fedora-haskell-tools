import Control.Monad (when)
import Data.List (sort)
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import System.Environment (getArgs)

import FedoraDists
import SimpleCmd (cmd_)
import Paths_fedora_haskell_tools (version)

import Options.Applicative.Simple

-- mock :: [String] -> IO String
-- mock os = cmd "mock" os

main :: IO ()
main = do
  args <- getArgs
  (_,run) <-
    simpleOptions (showVersion version) "Fedora Haskell mock chroot tool"
    "Mock chroot setup for doing Fedora Haskell building and checking.\nYou can also use any mock command without '--'."
    (pure ()) $
    do mapM_ addMockCmd commonMockCmds
       addCmd "check" check "Check all Haskell libs installable"
       addCmd "repoquery" repoquery "Repoquery"
       addCmd "help" (pure Help) "mock help"
       addCmd "mockcmds" (pure MockCmds) "List mock commands"
       when (length args >= 2) $
         let c = head args in
           when (c `elem` allMockCmds) $
           addCmd c (arbitrary c False "OPT" False) ("Run mock " ++ c ++ " command")
  run
  where
    addCmd c parse desc = addCommand c desc dispatch parse

    addMockCmd (com,needarg,var,externopt,desc) =
      addCmd com (arbitrary com needarg var externopt) desc

    -- (cmd, needarg, VAR, externalopts, desc)
    commonMockCmds = [ ("chroot", True, "CMD", True, "Exec command in chroot")
                     , ("clean", False, "OPT", False, "Clean chroot")
                     , ("init", False, "OPT", False, "Setup chroot")
                     , ("install", True, "PKG", False, "Install packages")
                     , ("rebuild", True, "SRPM", False, "Build package(s)")
                     , ("remove", True, "PKG", False, "Remove package")
                     , ("shell", False, "CMD", True, "Interactive shell command")
                     , ("update", False, "PKG", False, "Update chroot packages")
                     ]

allMockCmds :: [String]
allMockCmds = sort
  ["rebuild", "buildsrpm", "debug-config", "shell", "chroot",
    "clean", "scrub", "init", "installdeps", "install", "update",
    "remove", "orphanskill", "copyin", "copyout",
    "pm-cmd", "yum-cmd", "dnf-cmd", "snapshot", "remove-snapshot",
    "rollback-to", "umount", "mount"]

data Args = Check Dist
          | Repoquery Dist [String]
          | Arbitrary Dist String [String]
          | Help
          | MockCmds
  deriving Show

distArg :: Parser Dist
distArg = argument auto (metavar "DIST" <> help "Fedora release, eg fXY")

strArg :: String -> String -> Parser String
strArg var desc = strArgument (metavar var <> help desc)

check :: Parser Args
check = Check <$> distArg

repoquery :: Parser Args
repoquery = Repoquery <$> distArg
         <*> many (strArg "PKG..." "repoquery args")

arbitrary :: String -> Bool -> String -> Bool -> Parser Args
arbitrary c needarg var externopt =
  Arbitrary <$> distArg
         <*> pure cm
         <*> ((["--" | externopt] ++) <$> (if needarg then some else many) (strArg (var ++ "...") "mock command options and args"))
  where
    cm =
      case c of
        ('-':'-':_) -> c
        _ -> "--" ++ c

dispatch :: Args -> IO ()
dispatch (Check dist) =
  runMock dist "--dnf-cmd" ["install", "ghc*devel"]
dispatch (Repoquery dist ps) =
  runMock dist "--dnf-cmd" $ "repoquery" : ps
dispatch Help =
  mock_ ["--help"]
dispatch MockCmds =
  putStrLn $ unwords $ sort allMockCmds
dispatch (Arbitrary dist a as) =
  runMock dist a as

runMock :: Dist -> String -> [String] -> IO ()
runMock dist c cs = do
  let release = releaseVersion dist
      conf = "fedora-" ++ release ++ "-x86_64"
      root = conf ++ "-haskell"
  let opts = ["-r", conf, "--config-opts=root=" ++ root]
  mock_ $ opts ++ (c:cs)

mock_ :: [String] -> IO ()
mock_ = cmd_ "mock"

