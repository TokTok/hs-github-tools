{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe            (fromMaybe, listToMaybe)
import qualified Data.Text             as Text
import           Data.Yaml             (decodeFileThrow)
import qualified GitHub
import           GitHub.Tools.Settings (syncSettings, validateSettings)
import           System.Environment    (getArgs, lookupEnv)

main :: IO ()
main = do
    -- Get auth token from the $GITHUB_TOKEN environment variable.
    rawArgs <- getArgs
    let dryRun = "--dry-run" `elem` rawArgs
        args   = filter (/= "--dry-run") rawArgs
    lookupEnv "GITHUB_TOKEN" >>= \case
        Nothing -> fail "GITHUB_TOKEN environment variable must be set"
        Just (GitHub.OAuth . BS8.pack -> token) ->
            case args of
                orgFile:reposFile:repoFilter -> do
                    orgYaml <- decodeFileThrow orgFile
                    reposYaml <- decodeFileThrow reposFile
                    validateSettings reposYaml
                    syncSettings token orgYaml reposYaml (Text.pack . fromMaybe "" . listToMaybe $ repoFilter) dryRun
                _      -> fail "Usage: hub-settings [--dry-run] <org.yml> <repos.yml> [repoFilter]"
