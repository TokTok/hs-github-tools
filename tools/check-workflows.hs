-- | Check that a workflow file is a subset of a reference workflow file, or
-- that workflows' actions are all pinned in their actions.lock.
--
-- Usage: check-workflows [ISSUE_TEMPLATE|workflows] <ref.yml> <workflow.yml>...
--        check-workflows actions-lock <actions.lock|-> <workflow.yml>...
--
-- The reference workflow file is a YAML file that contains a workflow spec.
--
-- The workflow files must be a superset of the reference workflow spec, i.e.
-- the intersection of the reference workflow spec and the workflow spec must be
-- equal to the reference workflow spec.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main (main) where

import           Control.Monad              (forM_, unless, when)
import qualified Data.Aeson.Key             as Key
import qualified Data.Aeson.KeyMap          as KeyMap
import qualified Data.Algorithm.DiffContext as Diff
import           Data.Char                  (isHexDigit)
import           Data.Containers.ListUtils  (nubOrd)
import           Data.List                  (sort, (\\))
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text
import qualified Data.Vector                as V
import           Data.Yaml                  (ToJSON, Value (..),
                                             decodeFileThrow, encode)
import           GitHub.Types.IssueTemplate (IssueTemplate,
                                             issueTemplateIntersection,
                                             parseIssueTemplate)
import           GitHub.Types.Json          (removeNulls)
import           GitHub.Types.Workflow      (Spec, parseSpec, specIntersection)
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           System.FilePath            (takeFileName)
import qualified Text.PrettyPrint           as PP

mustParse :: (Value -> Either String a) -> Value -> IO a
mustParse parse inValue =
  case parse inValue of
    Left err -> fail err
    Right ok -> return ok

main :: IO ()
main = do
  files <- getArgs
  case files of
    "ISSUE_TEMPLATE":expectedYmlPath:actualYmlPaths -> do
      ok <- mapM (checkIssueTemplate expectedYmlPath) actualYmlPaths
      unless (and ok) exitFailure
    "workflows":expectedYmlPath:actualYmlPaths -> do
      ok <- mapM (checkWorkflow expectedYmlPath) actualYmlPaths
      unless (and ok) exitFailure
    "actions-lock":lockPath:workflowPaths -> do
      problems <- checkActionsLock lockPath workflowPaths
      mapM_ Text.putStrLn problems
      unless (null problems) $ do
        putStrLn "Run `gh actions-lock` to update .github/workflows/actions.lock."
        exitFailure
    _ -> do
      putStrLn "Usage: check-workflows [ISSUE_TEMPLATE|workflows] <expected.yml> <actual.yml>..."
      putStrLn "       check-workflows actions-lock <actions.lock|-> <workflow.yml>..."
      exitFailure

check :: (Eq a, ToJSON a) => (Value -> Either String a) -> (a -> a -> a) -> FilePath -> FilePath -> IO Bool
check parse intersect expectedYmlPath actualYmlPath = do
  ref <- mustParse parse =<< decodeFileThrow expectedYmlPath
  inValue <- decodeFileThrow actualYmlPath
  spec <- mustParse parse inValue
  let outValue = removeNulls spec
  when (removeNulls inValue /= outValue) $ do
    Text.putStrLn . Text.decodeUtf8 . encode $ outValue
    putStrLn "Input not fully parsable"
    exitFailure
  let intersection = ref `intersect` spec
  if intersection == ref
    then return True
    else do
      let intersectionYaml = Text.decodeUtf8 . encode . removeNulls $ intersection
      let refYaml = Text.decodeUtf8 . encode . removeNulls $ ref
      putStrLn $ actualYmlPath <> ": intersection not equal to reference spec " <> expectedYmlPath
      Text.putStrLn $ showDiff intersectionYaml refYaml
      return False

checkIssueTemplate :: FilePath -> FilePath -> IO Bool
checkIssueTemplate = check parseIssueTemplate issueTemplateIntersection

checkWorkflow :: FilePath -> FilePath -> IO Bool
checkWorkflow = check parseSpec specIntersection

showDiff :: Text -> Text -> Text
showDiff a b = Text.pack . PP.render . toDoc $ diff
  where
    toDoc = Diff.prettyContextDiff (PP.text "payload")
                                   (PP.text "value")
                                   (\(Diff.Numbered _ t) -> PP.text . Text.unpack $ t)
    diff = Diff.getContextDiff linesOfContext (Text.lines a) (Text.lines b)
    linesOfContext = Just 3

-- | The actions a workflow's steps use. Local actions (./), containers and
-- reusable workflows (jobs.<id>.uses) aren't locked.
stepActions :: Value -> [Text]
stepActions (Object workflow) = nubOrd $ sort
  [ uses
  | Just (Object jobs) <- [KeyMap.lookup "jobs" workflow]
  , Object job <- KeyMap.elems jobs
  , Just (Array steps) <- [KeyMap.lookup "steps" job]
  , Object step <- V.toList steps
  , Just (String uses) <- [KeyMap.lookup "uses" step]
  , not $ any (`Text.isPrefixOf` uses) ["./", "docker://"]
  ]
stepActions _ = []

-- | How actions.lock names a workflow.
workflowKey :: FilePath -> Text
workflowKey = (".github/workflows/" <>) . Text.pack . takeFileName

field :: Text -> Value -> Maybe Value
field k (Object o) = KeyMap.lookup (Key.fromText k) o
field _ _          = Nothing

texts :: Maybe Value -> [Text]
texts (Just (Array a)) = [t | String t <- V.toList a]
texts _                = []

-- | Whether a lockfile commit is a full SHA-1, written "sha1-<40 hex digits>".
isPinned :: Text -> Bool
isPinned commit = case Text.stripPrefix "sha1-" commit of
  Just sha -> Text.length sha == 40 && Text.all isHexDigit sha
  Nothing  -> False

-- | What's wrong with actions.lock (- if there is none) for these workflows:
-- like `gh actions-lock --verify-local`, every action must be pinned.
checkActionsLock :: FilePath -> [FilePath] -> IO [Text]
checkActionsLock lockPath workflowPaths = do
  used <- filter (not . null . snd)
    <$> mapM (\path -> (workflowKey path,) . stepActions <$> decodeFileThrow path) workflowPaths
  if lockPath == "-"
    then return ["Workflows use actions, but there is no .github/workflows/actions.lock." | not (null used)]
    else lockProblems used (map workflowKey workflowPaths) <$> decodeFileThrow lockPath

lockProblems :: [(Text, [Text])] -> [Text] -> Value -> [Text]
lockProblems used workflowKeys lock =
  [ key <> ": " <> ref <> " isn't in actions.lock"
  | (key, refs) <- used, ref <- refs \\ lockedFor key ]
  ++ [ key <> ": actions.lock lists " <> ref <> ", which the workflow doesn't use"
     | (key, refs) <- locked, key `elem` workflowKeys, ref <- refs \\ fromMaybe [] (lookup key used) ]
  ++ [ "actions.lock lists " <> key <> ", which doesn't exist"
     | (key, _) <- locked, key `notElem` workflowKeys ]
  ++ [ ref <> " has no pinned commit in actions.lock" | ref <- allRefs, not (pinned ref) ]
  where
    entries name = case field name lock of
      Just (Object o) -> [(Key.toText k, v) | (k, v) <- KeyMap.toList o]
      _               -> []
    locked = [(key, texts (Just v)) | (key, v) <- entries "workflows"]
    lockedFor key = fromMaybe [] (lookup key locked)
    dependencies = entries "dependencies"
    -- Transitive dependencies are listed under the actions that use them.
    allRefs = nubOrd . sort $ concatMap snd locked ++ concatMap (texts . field "uses" . snd) dependencies
    pinned ref = case field "commit" =<< lookup ref dependencies of
      Just (String commit) -> isPinned commit
      _                    -> False
