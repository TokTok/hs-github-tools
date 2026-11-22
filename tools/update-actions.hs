-- | Keep the GitHub Actions that a workspace's repositories use on the same
-- versions and commits.
--
-- Usage: update-actions bump <action> <ref>
--        update-actions lock
--        update-actions relock
--
-- Run it with `bazel run` in the workspace (toktok-stack). Its repositories
-- are the workspace and its submodules.
--
-- lock runs `gh actions-lock` once over all their workflows, then writes each
-- repository's .github/workflows/actions.lock from the result, so an action
-- has the same commit everywhere. It keeps the commits already pinned, and
-- relock moves them to their refs' current commits. The combined lock goes
-- in tools/project/actions.lock, which the workspace's tests check every
-- repository's lock against.
--
-- bump changes every use of an action (a repository like actions/checkout,
-- with any path inside it) to a new ref, then locks.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
module Main (main) where

import           Control.Monad            (filterM, forM, forM_, unless, when)
import           Data.Char                (isSpace)
import           Data.List                (isSuffixOf, sort)
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Data.Yaml                (decodeFileThrow)
import           GitHub.Types.ActionsLock (actionRepo, renameWorkflows,
                                           renderLock, repoLock,
                                           repoWorkflowKey)
import           System.Directory         (XdgDirectory (XdgCache), copyFile,
                                           createDirectoryIfMissing,
                                           doesDirectoryExist, doesFileExist,
                                           getCurrentDirectory, getXdgDirectory,
                                           listDirectory, removePathForcibly)
import           System.Environment       (getArgs, getEnvironment, lookupEnv)
import           System.Exit              (ExitCode (..), exitFailure)
import           System.FilePath          ((</>))
import           System.Process           (CreateProcess (..), proc,
                                           waitForProcess, withCreateProcess)

workflowsDir, lockFile, combinedLock :: FilePath
workflowsDir = ".github/workflows"
lockFile = workflowsDir </> "actions.lock"
combinedLock = "tools/project/actions.lock"

main :: IO ()
main = do
  root <- maybe getCurrentDirectory return =<< lookupEnv "BUILD_WORKSPACE_DIRECTORY"
  args <- getArgs
  repos <- findRepos root
  case args of
    ["bump", action, ref] -> do
      forM_ repos $ \repo ->
        mapM_ (bump (Text.pack action) (Text.pack ref) . ((repoDir root repo </> workflowsDir) </>)) =<< workflows root repo
      lock root repos []
    ["lock"] -> lock root repos []
    ["relock"] -> lock root repos ["--relock"]
    _ -> do
      putStrLn "Usage: update-actions bump <action> <ref>"
      putStrLn "       update-actions lock"
      putStrLn "       update-actions relock"
      exitFailure

-- | The workspace (".") and its submodules that have workflows.
findRepos :: FilePath -> IO [FilePath]
findRepos root = do
  gitmodules <- readIfExists $ root </> ".gitmodules"
  filterM (doesDirectoryExist . (</> workflowsDir) . repoDir root) $ "." :
    [ Text.unpack (Text.strip path)
    | line <- Text.lines gitmodules
    , Just path <- [Text.stripPrefix "path =" (Text.strip line)] ]

repoDir :: FilePath -> FilePath -> FilePath
repoDir root "."  = root
repoDir root repo = root </> repo

workflows :: FilePath -> FilePath -> IO [FilePath]
workflows root repo =
  sort . filter (\f -> any (`isSuffixOf` f) [".yml", ".yaml"]) <$> listDirectory (repoDir root repo </> workflowsDir)

-- | Change every `uses: <action>[/<path>]@<ref>` in a workflow to the new ref.
bump :: Text -> Text -> FilePath -> IO ()
bump action ref path = do
  old <- Text.readFile path
  let new = Text.unlines . map bumpLine . Text.lines $ old
  when (new /= old) $ do
    Text.writeFile path new
    putStrLn $ "bumped " <> path
  where
    bumpLine line = case Text.breakOn "uses:" line of
      (_, "") -> line
      (_, rest) ->
        let uses = Text.filter (`notElem` ['"', '\''])
                 . Text.takeWhile (\c -> not (isSpace c) && c /= '#')
                 . Text.dropWhile isSpace $ Text.drop 5 rest
            (pathAt, _) = Text.breakOnEnd "@" uses
        in if Text.null pathAt || actionRepo (Text.dropEnd 1 pathAt) /= Text.toLower action
             then line
             else Text.replace uses (pathAt <> ref) line

-- | Lock all repositories' workflows together: copy them into one scratch
-- repository with the current combined lock, run `gh actions-lock` there, and
-- give each repository its part of the result.
lock :: FilePath -> [FilePath] -> [String] -> IO ()
lock root repos flags = do
  scratch <- getXdgDirectory XdgCache "update-actions"
  removePathForcibly scratch
  createDirectoryIfMissing True $ scratch </> workflowsDir
  files <- concat <$> forM repos (\repo -> map (repo,) <$> workflows root repo)
  -- Repositories have workflows with the same names, so number the copies.
  let copies = zip [Text.pack (workflowsDir </> show i <> "-" <> file) | (i, (_, file)) <- zip [(1 :: Int) ..] files] files
      toRepo = Map.fromList [(copy, uncurry repoWorkflowKey f) | (copy, f) <- copies]
      toCopy = Map.fromList [(uncurry repoWorkflowKey f, copy) | (copy, f) <- copies]
  forM_ copies $ \(copy, (repo, file)) ->
    copyFile (repoDir root repo </> workflowsDir </> file) (scratch </> Text.unpack copy)
  hasCombined <- doesFileExist $ root </> combinedLock
  when hasCombined $
    Text.writeFile (scratch </> lockFile) . renderLock . renameWorkflows (`Map.lookup` toCopy)
      =<< decodeFileThrow (root </> combinedLock)

  environment <- getEnvironment
  let gh = (proc "gh" $ ["actions-lock", "--no-narrow", "--no-interactive", "--hostname", "github.com"] ++ flags)
        { cwd = Just scratch
        , env = Just $ ("GH_ACTIONS_LOCK_REPO_ROOT", scratch) : filter (("GH_ACTIONS_LOCK_REPO_ROOT" /=) . fst) environment
        }
  code <- withCreateProcess gh $ \_ _ _ -> waitForProcess
  unless (code == ExitSuccess) $ fail "gh actions-lock failed"

  combined <- renameWorkflows (`Map.lookup` toRepo) <$> decodeFileThrow (scratch </> lockFile)
  writeIfChanged (root </> combinedLock) $ renderLock combined
  forM_ repos $ \repo ->
    writeIfChanged (repoDir root repo </> lockFile) . renderLock $ repoLock repo combined

readIfExists :: FilePath -> IO Text
readIfExists path = do
  exists <- doesFileExist path
  if exists then Text.readFile path else return ""

writeIfChanged :: FilePath -> Text -> IO ()
writeIfChanged path new = do
  exists <- doesFileExist path
  old <- if exists then Just <$> Text.readFile path else return Nothing
  unless (old == Just new) $ do
    Text.writeFile path new
    putStrLn $ "wrote " <> path
