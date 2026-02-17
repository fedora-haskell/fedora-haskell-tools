{-# LANGUAGE CPP #-}

-- |
-- Module      :  Packages
-- Copyright   :  (C) 2017-2019  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
--
-- Explanation: Cloning and pulling package git repos

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,13,0))
#else
import Control.Applicative (optional, some, (<|>)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
                           , pure, (<$>), (<*>)
#endif
                           )
#endif
import Control.Monad (filterM, unless, when, (>=>))
import Control.Monad.Extra (mapMaybeM, unlessM, whenJustM)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isDigit)
import Data.Maybe
import Data.List
import Data.List.Split (splitOn)
import Distribution.Fedora.Branch (Branch(..), getLatestFedoraBranch,
                                   readBranch, showBranch)
import Network.HTTP.Simple
import Network.HTTP.Types
import SimplePrompt (promptEnter)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getHomeDirectory,
#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
                         listDirectory,
#else
                         getDirectoryContents,
#endif
                         setCurrentDirectory)
import System.FilePath ((</>), (<.>), takeFileName)
import System.IO (BufferMode(..), hIsTerminalDevice, hSetBuffering, stdout)
--import System.Posix.Env (getEnv)
import Text.CSV (parseCSV)

import SimpleCmd
#if !MIN_VERSION_simple_cmd(0,2,3)
import System.Exit
import qualified System.Process as P
#endif
import SimpleCmd.Git
import SimpleCmdArgs

import Paths_fedora_haskell_tools (version)
import RPM (buildRequires, haskellSrcPkgs, Package,
            repoquery, rpmspec, rqfnewline)
import Utils (checkPkgsGit, withCurrentDirectory)

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,5))
#else
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs (Just version) "Fedora Haskell packages tool"
    "Fedora packages maintenance tool" $
    subcommands . sort $
    [
      Subcommand "clone-new" "clone new packages" $
      cloneNew <$> branchArg
    , Subcommand "cblrpm" "Run cblrpm command" $
      cblrpm <$> strOptionWith 'c' "cmd" "CMD" "command to execute" <*> branchArg <*> pkgArgs
    , Subcommand "count" "count number of packages" $
      (repoqueryHaskellPkgs False >=> (print . length)) <$> branchArg
    , Subcommand "depends" "cabal-depends" $
      repoAction False (Output cabalDepends) <$> branchArg <*> pkgArgs
    , Subcommand "hackage-upload" "upload Hackage distro data" $
      hackageUpload <$> branchArg <*> switchRefresh
    , Subcommand "hackage-compare" "compare with Hackage distro data" $
      hackageCompare <$> branchArg <*> switchRefresh
    -- more or less the same as 'pushed'
    , Subcommand "head-origin" "packages with head in sync with origin" $
      headOrigin <$> branchArg <*> pkgArgs
    , Subcommand "leaf" "list leaf packages (slow!)" $
      leaves <$> switchWith 'v' "deps" "show also deps" <*> branchArg <*> pkgArgs
    , Subcommand "list" "list packages that BR ghc-Cabal-devel" $
      (repoqueryHaskellPkgs False >=> putStrList) <$> branchArg
    , Subcommand "missing" "missing dependency source packages" $
      missingDeps <$> branchArg <*> pkgArgs
    , Subcommand "new" "unbuilt packages" $
      (newPackages >=> putStrList) <$> branchArg
    , Subcommand "old-packages" "packages not in repoquery" $
      oldPackages <$> branchArg <*> pkgArgs
    , Subcommand "stackage-compare" "compare with stackage" $
      stackageCompare <$> streamOpt Nothing <*> stackageOpts <*> branchArg <*> pkgArgs
    , Subcommand "pushed" "show git pushed packages" $
      pushed <$> branchArg <*> pkgArgs
    , Subcommand "refresh" "cabal-rpm refresh" $
      refresh <$> switchWith 'n' "dry-run" "Show patch but don't apply" <*> branchArg <*> pkgArgs
    , Subcommand "remaining" "remaining packages to be built in TAG" $
      remaining <$> switchWith 'c' "count" "show many packages left" <*> strArg "TAG" <*> pkgArgs
    , Subcommand "subpkgs" "list subpackages" $
      repoAction True (Header True (\ p -> rpmspec [] (Just "%{name}-%{version}") (p <.> "spec") >>= putStrList)) <$> branchArg <*> pkgArgs
    , Subcommand "subpackaged" "list subpackaged libraries" $
      subpackaged <$> switchWith 'V' "show-versions" "Show versions" <*> branchArg <*> pkgArgs
    , Subcommand "tagged" "list koji DIST tagged builds" $
      listTagged_ <$> switchWith 's' "short" "list packages not builds" <*> strArg "TAG"
    , Subcommand "unbranched" "packages without this branch" $
      unbranched <$> branchArg <*> pkgArgs
    , Subcommand "unpushed" "show unpushed commits" $
      unpushed <$> switchWith 's' "short" "no log" <*> branchArg <*> pkgArgs
    , Subcommand "update" "cabal-rpm update" $
      update <$> keepOpt <*> streamOpt (Just "old") <*> branchArg <*> pkgArgs
    ]
  where
    branchArg :: Parser Branch
    branchArg = argumentWith branchM "BRANCH"

    branchM :: ReadM Branch
    branchM = maybeReader readBranch

    pkgArgs = some (removeSuffix "/" <$> strArg "PKG...")

    switchRefresh = switchWith 'r' "refresh" "repoquery --refresh"

    streamOpt mdesc = strOptionalWith 's' "stream" "STACKAGESTREAM" (fromMaybe "" mdesc +-+ "Stackage stream [" ++ ltsStream ++ "]") ltsStream

    ltsStream :: String
    ltsStream = "lts-24"

    stackageOpts :: Parser StkgOpt
    stackageOpts =
      flagWith' StkgMissing 'm' "missing" "only list missing packages" <|>
      flagWith StkgAll StkgOnly 'o' "only" "only Stackage packages"

    keepOpt = switchWith 'k' "keep-going" "Keep going after an error"

data StkgOpt = StkgAll | StkgOnly | StkgMissing
  deriving Eq

putStrList :: [String] -> IO ()
putStrList =
  putStr . unlines

cloneNew :: Branch -> IO ()
cloneNew branch =
  newPackages branch >>= repoAction_ True False (return ()) branch

-- FIXME option only to show PVP X.Y bumps or X.Y.Z bumps
stackageCompare :: String -> StkgOpt -> Branch -> [Package] -> IO ()
stackageCompare stream opt =
  repoAction True (Header False compareStackage)
  where
    compareStackage :: Package -> IO ()
    compareStackage p = do
      nvr <- cmd "fedpkg" ["verrel"]
      let hkg = removePrefix "ghc-" p
      (ok,_out,err) <- cmdFull "stack" ["--resolver" , stream, "list", hkg] ""
      -- FIXME rename to mstkgver?
      let stkg =
            if ok then
              case filter (not . ("Selected resolver:" `isPrefixOf`)) (lines err) of
                [nv] -> Just (removePrefix (hkg ++ "-") nv)
                _ -> Nothing
            else Nothing
      let same = isJust stkg && (fromJust stkg ++ "-") `isInfixOf` nvr
      unless same $
        if opt == StkgMissing
        then when (isNothing stkg) $ putStrLn p
        else
          if isNothing stkg
          then unless (opt == StkgOnly) $
               putStrLn $ stream ++ " missing: " ++ removePrefix "ghc-" p
          else do
            putStrLn nvr
            putStrLn $ replicate (length (dropVerrel nvr) + 1) ' ' ++ fromJust stkg +-+ "(" ++ stream ++ ")"

hackageUpload :: Branch -> Bool -> IO ()
hackageUpload hackageBranch refreshData = do
  branched <- getLatestFedoraBranch
  unless (branched == hackageBranch) $
    promptEnter "Not latest branch!  Please Enter to continue anyway"
  csv <- repoqueryHackageCSV
  home <- getHomeDirectory
  [username, password] <- map B.pack . words <$> readFile (home </> ".fedora/hackage.auth")
  req <- setRequestBasicAuth username password .
         setRequestBodyLBS (BL.pack csv) .
         addRequestHeader hContentType (B.pack "text/csv") .
         setRequestMethod methodPut <$>
         parseRequestThrow "https://hackage.haskell.org/distro/Fedora/packages.csv"
  resp <- httpLbs req
  BL.putStrLn $ getResponseBody resp
  where
    repoqueryHackageCSV :: IO String
    repoqueryHackageCSV = do
      pkgs <- repoqueryHackages hackageBranch
      -- Hackage csv chokes on a final newline
      intercalate "\n" . sort . map (replace "\"ghc-" "\"")  . lines <$>
        repoquery hackageBranch (["--repo=fedora", "--repo=updates", "--latest-limit=1", "--qf=\"%{name}\",\"%{version}\",\"https://src.fedoraproject.org/rpms/%{source_name}\"" ++ rqfnewline] ++ ["--refresh" | refreshData] ++ pkgs)

hackageCompare :: Branch -> Bool -> IO ()
hackageCompare hackageBranch refreshData =
  repoqueryHackages hackageBranch >>=
  compareHackage hackageBranch
  where
    compareHackage :: Branch -> [Package] -> IO ()
    compareHackage dist pkgs' = do
      req <- addRequestHeader hContentType (B.pack "text/csv") <$>
             parseRequestThrow "https://hackage.haskell.org/distro/Fedora/packages.csv"
      hck <- getResponseBody <$> httpLbs req
      let hackage = sort . either (error "Malformed Hackage csv") (map mungeHackage) $ parseCSV "packages.csv" (BL.unpack hck)
      repoquery dist (["--repo=fedora", "--repo=updates", "--latest-limit=1", "--qf=%{name},%{version}" ++ rqfnewline] ++ ["--refresh" | refreshData] ++ pkgs') >>=
        compareSets True hackage . sort . map mungeRepo . lines

    mungeHackage :: [String] -> PkgVer
    mungeHackage [n,v,_] = PV n v
    mungeHackage _ = error "Malformed Hackage csv"

    mungeRepo :: String -> PkgVer
    mungeRepo s | ',' `elem` s =
                  let (p,v) = break (== ',') s in
                    PV (removePrefix "ghc-" p) (tail v)
                | otherwise = error "Malformed repoquery output"

    compareSets :: Bool -> [PkgVer] -> [PkgVer] -> IO ()
    compareSets _ [] [] = return ()
    compareSets _ [] (f:fs) = do
      putStrLn (show f +-+ "(new)")
      compareSets False [] fs
    compareSets all' (h:hs) [] = do
      when all' $ putStrLn ("Removed:" +-+ show h)
      compareSets all' hs []
    compareSets all' (h:hs) (f:fs) | h == f = compareSets all' hs fs
                                   | h < f = do
                                       when all' $ putStrLn $ "Removed:" +-+ show h
                                       compareSets all' hs (f:fs)
                                   | h > f = do
                                       putStrLn $ show f +-+ "(new)"
                                       compareSets all' (h:hs) fs
                                   | otherwise = do
                                       putStrLn $ pvPkg h ++ ":" +-+ pvVer h +-+ "->" +-+ pvVer f
                                       compareSets all' hs fs

headOrigin :: Branch -> [Package] -> IO ()
headOrigin branch =
  repoAction False (Header False gitHeadAtOrigin) branch
  where
    gitHeadAtOrigin :: Package -> IO ()
    gitHeadAtOrigin pkg = do
      -- use gitDiffQuiet
      same <- gitBool "diff" ["--quiet", "origin" </> showBranch branch ++ "..HEAD"]
      when same $ putStrLn pkg

-- FIXME does not take static requires into account
leaves :: Bool -> Branch -> [Package] -> IO ()
leaves verb =
  repoAction True (Header verb checkLeafPkg)
  where
    -- FIXME: make a dependency cache
    checkLeafPkg :: Package -> IO ()
    checkLeafPkg pkg = do
      dir <- takeFileName <$> getCurrentDirectory
      let branchdir = dir /= pkg
          top = if branchdir then "../.." else ".."
          spec = pkg <.> "spec"
      subpkgs <- rpmspec ["--builtrpms"] (Just "%{name}") spec
      allpkgs <- listDirectory top
      let other = map (\ p -> top </> p </> (if branchdir then dir else "") </> p <.> "spec") $ allpkgs \\ [pkg]
      found <- filterM (dependsOn subpkgs) other
      if null found
        then putStrLn pkg
        else when verb $ putStrList found
        where
          dependsOn :: [Package] -> Package -> IO Bool
          dependsOn subpkgs p = do
            file <- doesFileExist p
            if file
              then do
              deps <- buildRequires p
              return $ any (`elem` deps) subpkgs
              else return False

missingDeps :: Branch -> [Package] -> IO ()
missingDeps dist =
  repoAction True (Output checkForMissingDeps) dist
  where
    checkForMissingDeps :: Package -> IO String
    checkForMissingDeps pkg = do
      dir <- takeFileName <$> getCurrentDirectory
      let top = if dir == pkg then ".." else "../.."
          spec = pkg <.> "spec"
      hasSpec <- doesFileExist spec
      if hasSpec
        then do
        deps <- buildRequires (pkg <.> "spec") >>= haskellSrcPkgs top dist
        unlines <$> filterM (noPkgDir top) deps
        else putStrLn ("no " ++ pkg ++ ".spec file found!") >> return ""
        where
          noPkgDir :: FilePath -> Package -> IO Bool
          noPkgDir top dep =
            not <$> doesDirectoryExist (top </> dep)

oldPackages :: Branch -> [Package] -> IO ()
oldPackages branch pkgs = do
  repopkgs <- repoqueryHaskellPkgs True branch
  putStrList (pkgs \\ repopkgs)

unpushed :: Bool -> Branch -> [Package] -> IO ()
unpushed nolog branch =
  repoAction True (Header False gitLogOneLine) branch
  where
    gitLogOneLine :: Package -> IO ()
    gitLogOneLine pkg = do
      out <- git "log" ["origin" </> showBranch branch ++ "..HEAD", "--pretty=oneline"]
      unless (null out) $
        putStrLn $ pkg ++ if nolog then "" else (unwords . map replaceHash . words) out
        where
          replaceHash h = if length h /= 40 then h else ":"

pushed :: Branch -> [Package] -> IO ()
pushed branch =
  repoAction True (Header False checkPushed) branch
  where
    checkPushed :: Package -> IO ()
    checkPushed pkg = do
      out <- git "log" ["origin" </> showBranch branch ++ "..HEAD", "--pretty=oneline"]
      when (null out) $ putStrLn pkg

data PkgVer = PV { pvPkg :: String, pvVer :: String}
  deriving (Eq)

instance Show PkgVer
  where
    show (PV p v) = p ++ "-" ++ v

instance Ord PkgVer
  where
    compare (PV p _) (PV p' _) = compare p p'

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b s@(x:xs) =
  if a `isPrefixOf` s
  then b ++ replace a b (drop (length a) s)
  else x:replace a b xs
replace _ _ [] = []

repoqueryHaskellPkgs :: Bool -> Branch -> IO [Package]
repoqueryHaskellPkgs verbose branch = do
  when verbose $ do
    tty <- hIsTerminalDevice stdout
    when tty $ warning "Getting packages from repoquery"
  let repo = "fedora"
      updates = ["updates"] -- maybeToList $ distUpdates dist
  bin <- words <$> repoquery branch (["--repo=" ++ repo ++ "-source"] ++ ["--repo=" ++ u  ++ "-source" | u <- updates] ++ ["--qf=%{name}" ++ rqfnewline, "--whatrequires", "ghc-Cabal-*"])
  when (null bin) $ error "No packages using ghc-Cabal-devel found!"
  return $ sort $ filter (not . isGhcXY) $ nub bin

isGhcXY :: String -> Bool
isGhcXY p = let prefix = head (splitOn "-" p)
            in all (\c -> isDigit c || c == '.') (removePrefix "ghc" prefix)

repoqueryHackages :: Branch -> IO [Package]
repoqueryHackages branch = do
  srcs <- repoqueryHaskellPkgs False branch
  libs <- repoqueryHaskellLibs False
  let binsrcs = filter (not . ("ghc-" `isPrefixOf`)) srcs
      sublibs = libs \\ map ("ghc-" ++) binsrcs
  return $ sort $ nub (delete "haskell-platform" srcs ++ sublibs)
  where
    repoqueryHaskellLibs :: Bool -> IO [Package]
    repoqueryHaskellLibs verbose = do
      when verbose $ putStrLn "Getting libraries from repoquery"
      let repo = "fedora" -- distRepo branched dist
          updates = ["updates"]
      bin <- words <$> repoquery branch (["--repo=" ++ repo] ++ ["--repo=" ++ u | u <- updates] ++ ["--qf=%{name}" ++ rqfnewline, "--whatprovides", "libHS*-ghc*.so()(64bit)"])
      when (null bin) $ error "No libHS*.so providers found!"
      return $ sort $ filter ("ghc-" `isPrefixOf`) $ nub bin

pagureListGhcHaskell :: IO [String]
pagureListGhcHaskell = do
  ps <- cmdLines "pagure" ["list", "--namespace", "rpms", "ghc*"]
  return $ filter (not . isGhcXY) ps

newPackages :: Branch -> IO [Package]
newPackages branch = do
  ps <- repoqueryHaskellPkgs True branch
  pps <- pagureListGhcHaskell
  local <- listDirectory "."
  filterM (\ d -> not <$> doesFileExist (d </> "dead.package")) $ nub (pps ++ ps) \\ (local ++ ["Agda-stdlib", "ghc", "ghc-rpm-macros", "ghc-srpm-macros", "ghc-srpm-macros-epel", "haskell-platform"])

haveSshKey :: IO Bool
haveSshKey = do
  home <- getHomeDirectory
  doesFileExist $ home </> ".ssh/id_ed25519"

data Action = Output (Package -> IO String) | Header Bool (Package -> IO ())

showHeader :: Action -> Bool
showHeader (Header b _) = b
showHeader (Output _) = False

repoAction :: Bool -> Action -> Branch -> [Package] -> IO ()
repoAction _ _ _ [] = return ()
repoAction needsSpec action branch (pkg:rest) = do
  withCurrentDirectory "." $ do
    when (showHeader action) $
      putStrLn $ "\n==" +-+ pkg +-+ showBranch branch +-+ "=="
    -- muser <- getEnv "USER"
    haveSSH <- haveSshKey
    fileExists <- doesFileExist pkg
    if fileExists
      then error $ pkg +-+ "is a file"
      else do
      dirExists <- doesDirectoryExist pkg
      unless dirExists $
        cmd_ "fedpkg" $ ["clone"] ++ ["-a" | not haveSSH] ++ ["-b", showBranch branch, pkg]
      setCurrentDirectory pkg
      pkggit <- do
        gd <- isGitDir "."
        if gd
          then checkPkgsGit
          else return False
      unless pkggit $
        error $ "not a Fedora pkg git dir!:" +-+ pkg
      actual <- gitBranch
      when (showBranch branch /= actual) $
        whenJustM (cmdMaybe "fedpkg" ["switch-branch", showBranch branch]) $ \ out ->
        when (showHeader action) $ putStrLn out
      currentBranch <- gitBranch
      when (showBranch branch == currentBranch) $
        unlessM (doesFileExist "dead.package") $ do
          let spec = pkg <.> "spec"
          hasSpec <- doesFileExist spec
          -- FIXME: silence for cmds that only output package names (eg unpushed -s)
          when (not hasSpec && showHeader action) $
            putStrLn "No spec file!"
          unless (needsSpec && not hasSpec) $
            case action of
              Header _ act -> act pkg
              Output act -> do
                out <- act pkg
                unless (null out) $ do
                  putStrLn $ "\n==" +-+ pkg +-+ showBranch branch +-+ "=="
                  putStrLn out
  repoAction needsSpec action branch rest

-- io independent of package
repoAction_ :: Bool -> Bool -> IO () -> Branch -> [Package] -> IO ()
repoAction_ header needsSpec action =
  repoAction needsSpec (Header header (const action))

-- compareRawhide :: Package -> IO ()
-- compareRawhide p = do
--   let spec = p <.> "spec"
--   nvr <- removeDisttag . unwords <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec
--   nvr' <- withBranch "master" $ do
--     haveSpec <- doesFileExist spec
--     unless haveSpec $ cmdSilent "git" ["pull"]
--     removeDisttag . unwords <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec
--   if nvr == nvr'
--     then putStrLn nvr
--     else do
--     putStrLn nvr
--     putStrLn nvr'
--   putStrLn ""
--   where
--     removeDisttag = reverse . tail . dropWhile (/= '.') . reverse

isFromHackage :: Package -> IO Bool
isFromHackage pkg =
  grep_ "hackage.haskell.org/package/" $ pkg <.> "spec"

update :: Bool -> String -> Branch -> [Package] -> IO ()
update keep stream =
  repoAction True (Header True doUpdate)
  where
    doUpdate :: Package -> IO ()
    doUpdate pkg = do
      hckg <- isFromHackage pkg
      if hckg
        then do
        ok <- cmdBool "cabal-rpm" ["update", "-o", stream]
        unless (ok || keep) $
          error' $ "cabal-rpm update failed for: " ++ pkg
        else putStrLn "skipping since not hackage"

refresh ::Bool -> Branch -> [Package] -> IO ()
refresh dryrun =
  repoAction True (Header True refreshPkg)
  where
    refreshPkg :: Package -> IO ()
    refreshPkg pkg = do
      hckg <- isFromHackage pkg
      if hckg
        then cmd_ "cabal-rpm" $ "refresh" : ["--dry-run" | dryrun]
        else putStrLn "skipping since not hackage"

listTagged_ :: Bool -> String -> IO ()
listTagged_ short tag =
  listTagged short tag >>= putStrList

listTagged :: Bool -> String -> IO [String]
listTagged short tag = do
  builds <- map (head . words) <$> cmdLines "koji" ["list-tagged", "--quiet", tag]
  return $ nub $ map (if short then dropVerrel else id) builds

dropVerrel :: String -> String
dropVerrel nvr =
  let parts = splitOn "-" nvr in
    intercalate "-" $ take (length parts - 2) parts

remaining :: Bool -> String -> [Package] -> IO ()
remaining count tag pkgs = do
  built <- listTagged True tag
  let left = pkgs \\ built
  if count
    then print $ length left
    else cmd_ "rpmbuild-order" $ ["sort", "-p"] ++ left

cabalDepends :: Package -> IO String
cabalDepends p = do
  hckg <- isFromHackage p
  if hckg then do
    vr <- removePrefix "ghc-" . head <$>
      rpmspec ["--srpm"] (Just "%{name}-%{version}") (p <.> "spec")
    setCurrentDirectory vr
    cmdQuiet "cabal-depends" ["--not-build", "--unique"]
    else return ""

cblrpm :: String -> Branch -> [Package] -> IO ()
cblrpm "" = error "CMD string must be given"
cblrpm cs =
  repoAction True (Header True doCblRpm)
  where
    doCblRpm :: Package -> IO ()
    doCblRpm p = do
    hckg <- isFromHackage p
    when hckg $
      cmd_ "cblrpm" [cs]

unbranched :: Branch -> [Package] -> IO ()
unbranched branch =
  mapM_ checkBranch
  where
    checkBranch :: Package -> IO ()
    checkBranch pkg =
      withCurrentDirectory pkg $ do
      dead <- doesFileExist "dead.package"
      unless dead $ do
        havebranch <- gitBool "show-ref" ["--verify", "--quiet", "refs/heads/" ++ showBranch branch]
        unless havebranch $ do
          remotebranch <- gitBool "ls-remote" ["--exit-code", "--refs", "origin", showBranch branch]
          unless remotebranch $
            putStrLn pkg

subpackaged ::Bool -> Branch -> [Package] -> IO ()
subpackaged versions branch pkgs = do
  repoAction True (Output listSubpkgs) branch pkgs
  where
    listSubpkgs pkg = do
      msubpkgs <- fmap (drop 2 . words) <$> cmdMaybe "grep" ["%global subpkgs", pkg <.> "spec"]
      case msubpkgs of
        Nothing -> return ""
        Just subpkgs ->
          intercalate "\n" <$> mapMaybeM expand subpkgs
      where
        expand subpkg =
          let macro = (init . drop 2) subpkg in
            fmap (removeVersion . last . words) <$> cmdMaybe "grep" ["%global " ++ macro, pkg <.> "spec"]

        removeVersion nv =
          if versions then nv else init (dropWhileEnd (/= '-') nv)

#if !MIN_VERSION_simple_cmd(0,2,2)
-- | 'gitBool c args' runs git command and return result
gitBool :: String -- ^ git command
        -> [String] -- ^ arguments
        -> IO Bool -- ^ result
gitBool c args = do
  mout <- cmdMaybe "git" (c:args)
  return $ isJust mout
#endif

#if !MIN_VERSION_simple_cmd(0,2,3)
cmdFull :: String -> [String] -> String -> IO (Bool, String, String)
cmdFull c args input = do
  (ret, out, err) <- P.readProcessWithExitCode c args input
  return (ret == ExitSuccess, removeTrailingNewline out, removeTrailingNewline err)
  where
    removeTrailingNewline :: String -> String
    removeTrailingNewline "" = ""
    removeTrailingNewline str' =
      if last str' == '\n'
      then init str'
      else str'
#endif
