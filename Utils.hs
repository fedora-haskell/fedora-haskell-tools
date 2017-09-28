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

module Utils where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Data.List (isInfixOf, {-isPrefixOf,-} stripPrefix)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcess, readProcessWithExitCode, rawSystem)

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

cmdStdIn_ :: String -> [String] -> String -> IO ()
cmdStdIn_ c as inp = cmdStdIn c as inp >> return ()

cmd_ :: String -> [String] -> IO ()
cmd_ c as = do
  ret <- rawSystem c as
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "\"" ++ c +-+ unwords as ++ "\" failed with exit code" +-+ show n

-- dry-run
cmdN :: String -> [String] -> IO ()
cmdN c as = putStrLn $ c +-+ show as

cmdAssert :: String -> String -> [String] -> IO ()
cmdAssert msg c as = do
  ret <- rawSystem c as
  case ret of
    ExitSuccess -> return ()
    ExitFailure _ -> error msg

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

sudo :: String -> [String] -> IO ()
sudo c as = cmdlog "sudo" (c:as)

shell :: String -> IO String
shell c = cmd "sh" ["-c", c]

kojiLatestPkg :: String -> String -> IO String
kojiLatestPkg dist pkg = do
  res <- words <$> cmd "koji" ["latest-pkg", "--quiet", dist, pkg]
  return $ if null res then "" else head res

kojiWaitPkg :: String -> String -> IO ()
kojiWaitPkg dist pkg = --do
  cmd_ "koji" ["wait-repo", dist, "--build=" ++ pkg]
  --putStrLn ""

kojiBuilding :: String -> String -> IO Bool
kojiBuilding pkg build = do
  tasks <- lines <$> cmd "koji" ["list-tasks", "--mine", "--quiet"]
  return $ any (build `isInfixOf`) tasks || any (("/" ++ pkg ++ ":") `isInfixOf`) tasks

-- parseKojiTask :: [String] -> Maybe String
-- parseKojiTask [] = Nothing
-- parseKojiTask (l:ls) | "Created task:" `isPrefixOf` l = Just $ removePrefix "Created task: " l
--                       | otherwise = parseKojiTask ls

notInKoji :: String -> FilePath -> String -> String -> IO Bool
notInKoji branch topdir tag pkg = do
  latest <- kojiLatestPkg tag pkg
  local <- cmd "fedpkg" ["--path", topdir </> pkg </> branch, "verrel"]
  if latest == local
    then kojiWaitPkg tag latest >> return False
    else return True

removePrefix :: String -> String -> String
removePrefix prefix orig =
  fromMaybe (error prefix +-+ "is not prefix of" +-+ orig) $ stripPrefix prefix orig

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
