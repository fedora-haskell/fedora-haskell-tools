import Data.List (sort)
import System.Environment (getArgs)

import FedoraDists
import SimpleCmd (cmd_)
import Paths_fedora_haskell_tools (version)

import SimpleCmdArgs
import Options.Applicative
#if (defined(MIN_VERSION_optparse_applicative) && MIN_VERSION_optparse_applicative(0,13,0))
--import Data.Semigroup ((<>))
#endif

-- mock :: [String] -> IO String
-- mock os = cmd "mock" os

main :: IO ()
main = do
  args <- getArgs
  simpleCmdArgs (Just version) "Fedora Haskell mock chroot tool"
    "Mock chroot setup for doing Fedora Haskell building and checking.\nYou can also use any mock command without '--'." $ subcommands $
    map mockCmd commonMockCmds ++
    [ Subcommand "check" (check <$> distArg) "Check all Haskell libs installable"
    , Subcommand "repoquery" (repoquery <$> distArg <*> many (strArg "PKG...")) "Repoquery"
    , Subcommand "help" (pure $ mock_ ["--help"]) "mock help"
    , Subcommand "mockcmds" (pure $ putStrLn $ unwords $ sort allMockCmds) "List mock commands"
    ] ++
    [ Subcommand c (runMock <$> distArg <*> optCmdArg <*> some (strArg "OPT")) ("Run mock " ++ c ++ " command") | length args >= 2, let c = head args, c `elem` allMockCmds]
  where
    mockCmd :: (String, Bool, String, Bool, String) -> Subcommand
    mockCmd (com,needarg,var,externopt,desc) =
      Subcommand com (runMock <$> distArg <*> optCmdArg <*> arbitrary needarg var externopt) desc

    optCmdArg =
      mkOpt <$> strArg "CMD"
      where
        mkOpt c =
          case c of
            ('-':'-':_) -> c
            _ -> "--" ++ c

    arbitrary :: Bool -> String -> Bool -> Parser [String]
    arbitrary needarg var externopt =
      (["--" | externopt] ++) <$> (if needarg then some else many) (strArg (var ++ "..."))

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

distArg :: Parser Dist
distArg = argument auto (metavar "DIST")

check :: Dist -> IO ()
check dist = runMock dist "--dnf-cmd" ["install", "ghc*devel"]

repoquery :: Dist -> [String] -> IO ()
repoquery dist ps = runMock dist "--dnf-cmd" $ "repoquery" : ps


runMock :: Dist -> String -> [String] -> IO ()
runMock dist c cs = do
  let conf = distRepo dist ++ "-" ++ releaseVersion dist ++ "-x86_64"
      root = conf ++ "-haskell"
  let opts = ["-r", conf, "--config-opts=root=" ++ root]
  mock_ $ opts ++ (c:cs)

mock_ :: [String] -> IO ()
mock_ = cmd_ "mock"

