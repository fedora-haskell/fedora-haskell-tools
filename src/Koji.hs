-- |
-- Module      :  Koji
-- Copyright   :  (C) 2016-2019  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  *nix
--
-- Explanation: Koji commands

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Koji (kojiBuilding,
             kojiCheckFHBuilt,
             kojicmd,
             kojiLatestPkg,
             kojiListPkgs,
             kojiWaitPkg,
             notInKoji,
             rpkg,
             rpkgBuild
            ) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.List (isInfixOf, isPrefixOf)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), (<.>))
import System.Process (readProcessWithExitCode, rawSystem)

import FedoraDists (Dist, kojicmd, rpkg)
import SimpleCmd (cmd, cmd_, cmdBool, cmdLines, grep_, logMsg,
                  removePrefix, removeStrictPrefix, warning, (+-+))
import Dist (distTag, distTarget)
import RPM (pkgDir)
import Utils (setTermTitle)

kojisafe :: Dist -> String -> [String] -> IO String
kojisafe dist c as =
  cmdFragile (kojicmd dist) (c:as)

kojiLatestPkg :: Dist -> String -> IO (Maybe String)
kojiLatestPkg dist pkg = do
  res <- words <$> kojisafe dist "latest-pkg" ["--quiet", distTag dist, pkg]
  return $ if null res then Nothing else Just $ head res

kojiWaitPkg :: FilePath -> Dist -> String -> IO ()
kojiWaitPkg topdir dist nvr = do
  let fhbuilt = topdir </> ".fhbuilt"
      tag = distTag dist
  already <- kojiCheckFHBuilt topdir nvr
  unless already $ do
    logMsg $ "Waiting for" +-+ nvr +-+ "in" +-+ tag
    setTermTitle $ '*' : removePrefix "ghc-" nvr
    cmdFragile_ (kojicmd dist) ["wait-repo", tag, "--build=" ++ nvr]
    appendFile fhbuilt $ nvr ++ "\n"

kojiCheckFHBuilt :: FilePath -> String -> IO Bool
kojiCheckFHBuilt topdir nvr = do
  let fhbuilt = topdir </> ".fhbuilt"
  grep_ nvr fhbuilt

kojiBuilding :: String -> String -> Dist -> IO Bool
kojiBuilding pkg build dist = do
  tasks <- lines <$> kojisafe dist "list-tasks" ["--mine", "--quiet"]
  return $ any (build `isInfixOf`) tasks || any (("/" ++ pkg ++ ":") `isInfixOf`) tasks

-- parseKojiTask :: [String] -> Maybe String
-- parseKojiTask [] = Nothing
-- parseKojiTask (l:ls) | "Created task:" `isPrefixOf` l = Just $ removeStrictPrefix "Created task: " l
--                       | otherwise = parseKojiTask ls

notInKoji :: String -> FilePath -> Dist -> String -> IO Bool
notInKoji branch topdir dist pkg = do
  latest <- kojiLatestPkg dist pkg
  pkgpath <- pkgDir pkg branch topdir
  let spec = pkg <.> "spec"
  specExists <- doesFileExist $ pkgpath </> spec
  if not specExists
    then error $ spec +-+ "not found"
    else do
    local <- cmd (rpkg dist) ["--path", pkgpath, "verrel"]
    if latest == Just local
      then kojiWaitPkg topdir dist local >> return False
      else return True

kojiListPkgs :: Dist -> IO [String]
kojiListPkgs dist =
  words <$> cmd (kojicmd dist) ["list-pkgs", "--tag=" ++ distTag dist]

rpkgBuild :: FilePath -> Dist -> String -> Bool -> IO ()
rpkgBuild topdir dist nvr waitrepo = do
  giturl <- cmd (rpkg dist) ["giturl"]
  setTermTitle $ removePrefix "ghc-" nvr
  out <- cmd (kojicmd dist) ["build", "--nowait", "--fail-fast", distTarget dist, giturl]
  putStrLn out
  let task = last . words . head $ lines out
  start <- getCurrentTime
  success <- kojiWatchTask dist task
  if success
    then do
    logMsg $ nvr ++ " built"
    when waitrepo $ kojiWaitPkg topdir dist nvr
    else do
    now <- getCurrentTime
    -- koji srpms typically take 2 minutes
    let countdown = 120 - round (diffUTCTime now start) :: Int
    when (countdown >= 0) $
      cmd_ "sleep" [show countdown]

data TaskState = TaskOpen | TaskFailed | TaskClosed | TaskFree

kojiWatchTask :: Dist -> String -> IO Bool
kojiWatchTask dist task = do
  res <- cmdBool (kojicmd dist) ["watch-task", task]
  if res then return True
    else do
    ti <- kojiTaskInfo
    case ti of
      TaskClosed -> return True
      TaskFailed -> error "Task failed!"
      _ -> kojiWatchTask dist task
      where
        kojiTaskInfo :: IO TaskState
        kojiTaskInfo = do
          info <- cmdLines (kojicmd dist) ["taskinfo", task]
          let state = removeStrictPrefix "State: " <$> filter ("State: " `isPrefixOf`) info
          return $
            case state of
              ["open"] -> TaskOpen
              ["failed"] -> TaskFailed
              ["closed"] -> TaskClosed
              ["free"] -> TaskFree
              _ -> error "unknown task state!"

cmdFragile :: String -> [String] -> IO String
cmdFragile c as = do
  (ret, out, err) <- readProcessWithExitCode c as ""
  case ret of
    ExitSuccess -> return out
    ExitFailure n -> do
      unless (null out) $ putStrLn out
      when (null (out ++ err)) $
        warning $ "\"" ++ c +-+ unwords as ++ "\"" +-+ "failed with status" +-+ show n
      unless (null err) $ warning err
      threadDelay 2000000
      cmdFragile c as

cmdFragile_ :: String -> [String] -> IO ()
cmdFragile_ c as = do
  ret <- rawSystem c as
  case ret of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      warning $ "retrying \"" ++ c +-+ unwords as ++ "\""
      threadDelay 2000000
      cmdFragile_ c as

