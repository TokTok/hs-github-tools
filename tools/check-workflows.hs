-- | Check that a workflow file is a subset of a reference workflow file.
--
-- Usage: check-workflows <ref.yml> <workflow.yml>...
--
-- The reference workflow file is a YAML file that contains a workflow spec.
--
-- The workflow files must be a superset of the reference workflow spec, i.e.
-- the intersection of the reference workflow spec and the workflow spec must be
-- equal to the reference workflow spec.
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad              (forM_, unless, when)
import qualified Data.Algorithm.DiffContext as Diff
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text
import           Data.Yaml                  (ToJSON, Value (..),
                                             decodeFileThrow, encode)
import           GitHub.Types.IssueTemplate (IssueTemplate,
                                             issueTemplateIntersection,
                                             parseIssueTemplate)
import           GitHub.Types.Json          (removeNulls)
import           GitHub.Types.Workflow      (Spec, parseSpec, specIntersection)
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
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
    _ -> do
      putStrLn "Usage: check-workflows [ISSUE_TEMPLATE|workflows] <expected.yml> <actual.yml>..."
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
                                   (PP.text . Text.unpack)
    diff = Diff.getContextDiff linesOfContext (Text.lines a) (Text.lines b)
    linesOfContext = 3
