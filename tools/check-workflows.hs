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
import           Data.Yaml                  (Value (..), decodeFileThrow,
                                             encode)
import           GitHub.Types.Workflow      (Spec, parseSpec, removeNulls,
                                             specIntersection)
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import qualified Text.PrettyPrint           as PP

loadSpec :: FilePath -> IO Value
loadSpec = decodeFileThrow

mustParseSpec :: Value -> IO Spec
mustParseSpec inValue =
  case parseSpec inValue of
    Left err -> fail err
    Right ok -> return ok

main :: IO ()
main = do
  files <- getArgs
  case files of
    refYmlPath:workflowYmlPaths -> do
      ok <- mapM (checkWorkflow refYmlPath) workflowYmlPaths
      unless (and ok) exitFailure
    _ -> do
      putStrLn "Usage: check-workflows <ref.yml> <workflow.yml>..."
      exitFailure

checkWorkflow :: FilePath -> FilePath -> IO Bool
checkWorkflow refYmlPath workflowYmlPath = do
  ref <- mustParseSpec =<< loadSpec refYmlPath
  inValue <- loadSpec workflowYmlPath
  spec <- mustParseSpec inValue
  let outValue = removeNulls spec
  when (removeNulls inValue /= outValue) $ do
    Text.putStrLn . Text.decodeUtf8 . encode $ outValue
    putStrLn "Input not fully parseable"
    exitFailure
  let intersection = specIntersection ref spec
  if intersection == ref
    then return True
    else do
      let intersectionYaml = Text.decodeUtf8 . encode . removeNulls $ intersection
      let refYaml = Text.decodeUtf8 . encode . removeNulls $ ref
      putStrLn $ workflowYmlPath <> ": intersection not equal to reference spec " <> refYmlPath
      Text.putStrLn $ showDiff intersectionYaml refYaml
      return False

showDiff :: Text -> Text -> Text
showDiff a b = Text.pack . PP.render . toDoc $ diff
  where
    toDoc = Diff.prettyContextDiff (PP.text "payload")
                                   (PP.text "value")
                                   (PP.text . Text.unpack)
    diff = Diff.getContextDiff linesOfContext (Text.lines a) (Text.lines b)
    linesOfContext = 3
