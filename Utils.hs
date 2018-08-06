-- |
-- Module      :  Cmd
-- Copyright   :  (C) 2014  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  *nix
--
-- Explanation: system/shell command utils

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Utils (checkPkgsGit,
              cmd,
              cmd_,
              cmdBool,
              cmdFragile,
              cmdFragile_,
              cmdlog,
              cmdMaybe,
              cmdN,
              cmdSilent,
              cmdStdErr,
              error_,
              git,
              git_,
              gitBranch,
              grep,
              logMsg,
              maybeRemovePrefix,
              removePrefix,
              removeSuffix,
              singleLine,
              sudo,
              withCurrentDirectory,
              (+-+) ) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess, readProcessWithExitCode, rawSystem)

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,3))
import System.Directory (withCurrentDirectory)
#else
import Control.Exception (bracket)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
#endif

infixr 4 +-+
(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

cmdStdIn :: String -> [String] -> String -> IO String
cmdStdIn c as inp = removeTrailingNewline <$> readProcess c as inp

cmd :: String -> [String] -> IO String
cmd c as = cmdStdIn c as ""

removeTrailingNewline :: String -> String
removeTrailingNewline "" = ""
removeTrailingNewline str =
  if last str == '\n'
  then init str
  else str

singleLine :: String -> String
singleLine "" = ""
singleLine s = (head . lines) s

cmdMaybe :: String -> [String] -> IO (Maybe String)
cmdMaybe c as = do
  (ret, out, _err) <- readProcessWithExitCode c as ""
  case ret of
    ExitSuccess -> return $ Just $ removeTrailingNewline out
    ExitFailure _ -> return Nothing

cmdStdErr :: String -> [String] -> IO (String, String)
cmdStdErr c as = do
  (_ret, out, err) <- readProcessWithExitCode c as ""
  return (removeTrailingNewline out, removeTrailingNewline err)

-- cmdBoolStd :: String -> [String] -> IO (Bool, String)
-- cmdBoolStd c as = do
--   (ret, out, err) <- readProcessWithExitCode c as ""
--   case ret of
--     ExitSuccess -> return (True, out)
--     ExitFailure n -> hPutStrLn stderr ("\"" ++ c +-+ unwords as ++ "\"" +-+ "failed with status" +-+ show n ++ "\n" ++ err) >> return (False, out)

cmdStdIn_ :: String -> [String] -> String -> IO ()
cmdStdIn_ c as inp = void $ cmdStdIn c as inp

cmd_ :: String -> [String] -> IO ()
cmd_ c as = do
  ret <- rawSystem c as
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "\"" ++ c +-+ unwords as ++ "\" failed with exit code" +-+ show n

cmdFragile :: String -> [String] -> IO String
cmdFragile c as = do
  (ret, out, err) <- readProcessWithExitCode c as ""
  case ret of
    ExitSuccess -> return out
    ExitFailure n -> do
      hPutStrLn stderr $ "\"" ++ c +-+ unwords as ++ "\"" +-+ "failed with status" +-+ show n ++ "\n" ++ err
      threadDelay 2000000
      cmdFragile c as

cmdFragile_ :: String -> [String] -> IO ()
cmdFragile_ c as = do
  ret <- rawSystem c as
  case ret of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      hPutStrLn stderr $ "retrying \"" ++ c +-+ unwords as ++ "\""
      threadDelay 2000000
      cmdFragile_ c as

-- dry-run
cmdN :: String -> [String] -> IO ()
cmdN c as = putStrLn $ c +-+ unwords as

--cmdAssert :: String -> String -> [String] -> IO ()
--cmdAssert msg c as = do
--  ret <- rawSystem c as
--  case ret of
--    ExitSuccess -> return ()
--    ExitFailure _ -> error msg

cmdlogStdIn :: String -> [String] -> String -> IO ()
cmdlogStdIn c as inp = do
  date <- cmd "date" ["+%T"]
  putStrLn $ date +-+ c +-+ unwords as
  cmdStdIn_ c as inp

cmdlog :: String -> [String] -> IO ()
cmdlog c as = cmdlogStdIn c as ""

logMsg :: String -> IO ()
logMsg msg = do
  date <- cmd "date" ["+%T"]
  putStrLn $ date +-+ msg

cmdBool :: String -> [String] -> IO Bool
cmdBool c as = do
  ret <- rawSystem c as
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

-- hide stdout
cmdSilent :: String -> [String] -> IO ()
cmdSilent c as = do
  (ret, _, err) <- readProcessWithExitCode c as ""
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "\"" ++ c +-+ unwords as ++ "\"" +-+ "failed with status" +-+ show n ++ "\n" ++ err

sudo :: String -> [String] -> IO ()
sudo c as = cmdlog "sudo" (c:as)

--shell :: String -> IO String
--shell c = cmd "sh" ["-c", c]

removePrefix :: String -> String -> String
removePrefix prefix orig =
  fromMaybe (error prefix +-+ "is not prefix of" +-+ orig) $ stripPrefix prefix orig

maybeRemovePrefix :: String -> String -> String
maybeRemovePrefix prefix orig =
  fromMaybe orig $ stripPrefix prefix orig

removeSuffix :: String -> String -> String
removeSuffix suffix orig =
  fromMaybe orig $ stripSuffix suffix orig
  where
    stripSuffix sf str = reverse <$> stripPrefix (reverse sf) (reverse str)

error_ :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error_ = errorWithoutStackTrace
#else
error_ = error
#endif

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,3))
#else
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action
#endif

checkPkgsGit :: IO Bool
checkPkgsGit =
  grep "\\(pkgs\\|src\\)." ".git/config"

git :: String -> [String] -> IO String
git c as =
  cmd "git" ("--no-pager":c:as)

git_ :: String -> [String] -> IO ()
git_ c as =
  cmd_ "git" ("--no-pager":c:as)

gitBranch :: IO String
gitBranch =
  removePrefix "* " . head . filter (isPrefixOf "* ") . lines <$> cmd "git" ["branch"]

grep :: String -> FilePath -> IO Bool
grep pat file =
  cmdBool "grep" ["-q", pat, file]

