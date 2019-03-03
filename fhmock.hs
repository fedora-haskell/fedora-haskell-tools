-- |
-- Module      :  Mock
-- Copyright   :  (C) 2018-2019  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: Updating of Upstream Release Monitoring bugs

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

import Data.List (sort)
import System.Environment (getArgs)

import Dist (distArg)
import Paths_fedora_haskell_tools (version)

import FedoraDists (Dist, distRepo, releaseVersion)
import SimpleCmd (cmd_)
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
    [ Subcommand "check" "Check all Haskell libs installable" $
      check <$> distArg
    , Subcommand "repoquery" "Repoquery" $
      repoquery <$> distArg <*> many (strArg "PKG...")
    , Subcommand "help" "mock help" $ pure $ mock_ ["--help"]
    , Subcommand "mockcmds" "List mock commands" $
      pure $ putStrLn $ unwords $ sort allMockCmds
    ] ++
    [ Subcommand c ("Run mock " ++ c ++ " command") (runMock <$> distArg <*> optCmdArg <*> some (strArg "OPT")) | length args >= 2, let c = head args, c `elem` allMockCmds]
  where
    mockCmd :: (String, Bool, String, Bool, String) -> Subcommand
    mockCmd (com,needarg,var,externopt,desc) =
      Subcommand com desc $
      runMock <$> distArg <*> optCmdArg <*> arbitrary needarg var externopt

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

