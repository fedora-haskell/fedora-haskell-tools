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
              error',
              withCurrentDirectory) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,3))
import System.Directory (withCurrentDirectory)
#else
import Control.Exception (bracket)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
#endif

import SimpleCmd.Git (grepGitConfig)


-- singleLine :: String -> String
-- singleLine "" = ""
-- singleLine s = (head . lines) s

-- cmdBoolStd :: String -> [String] -> IO (Bool, String)
-- cmdBoolStd c as = do
--   (ret, out, err) <- readProcessWithExitCode c as ""
--   case ret of
--     ExitSuccess -> return (True, out)
--     ExitFailure n -> hPutStrLn stderr ("\"" ++ c +-+ unwords as ++ "\"" +-+ "failed with status" +-+ show n ++ "\n" ++ err) >> return (False, out)

error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
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
  grepGitConfig "\\(pkgs\\|src\\)."

-- git :: String -> [String] -> IO String
-- git c as =
--   cmd "git" ("--no-pager":c:as)

-- git_ :: String -> [String] -> IO ()
-- git_ c as =
--   cmd_ "git" ("--no-pager":c:as)
