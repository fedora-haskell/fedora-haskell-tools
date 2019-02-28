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
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode, rawSystem)

import FedoraDists (Dist, distTag, distTarget, kojicmd, rpkg)
import RPM (pkgDir)
import SimpleCmd (cmd, cmd_, cmdBool, grep_, logMsg, (+-+))

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
    putStrLn $ "Waiting for" +-+ nvr +-+ "in" +-+ tag
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
  local <- cmd (rpkg dist) ["--path", pkgpath, "verrel"]
  if latest == Just local
    then kojiWaitPkg topdir dist local >> return False
    else return True

kojiListPkgs :: Dist -> IO [String]
kojiListPkgs dist =
  words <$> cmd (kojicmd dist) ["list-pkgs", "--tag=" ++ distTag dist]

rpkgBuild :: FilePath -> Dist -> Maybe String -> String -> Bool -> IO ()
rpkgBuild topdir dist mtarget nvr waitrepo = do
  giturl <- cmd (rpkg dist) ["giturl"]
  let target = fromMaybe (distTarget dist) mtarget
  out <- cmd (kojicmd dist) ["build", "--nowait", "--fail-fast", target, giturl]
  putStrLn out
  let task = last . words . head $ lines out
  start <- getCurrentTime
  success <- cmdBool "koji" ["watch-task", task]
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

cmdFragile :: String -> [String] -> IO String
cmdFragile c as = do
  (ret, out, err) <- readProcessWithExitCode c as ""
  case ret of
    ExitSuccess -> return out
    ExitFailure n -> do
      unless (null out) $ putStrLn out
      when (null (out ++ err)) $
        hPutStrLn stderr $ "\"" ++ c +-+ unwords as ++ "\"" +-+ "failed with status" +-+ show n
      unless (null err) $ hPutStrLn stderr err
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

