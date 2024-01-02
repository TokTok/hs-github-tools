{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Yaml             (decodeFileThrow)
import qualified GitHub
import           GitHub.Tools.Settings (syncSettings)
import           System.Environment    (getArgs, lookupEnv)

main :: IO ()
main = do
    -- Get auth token from the $GITHUB_TOKEN environment variable.
    args <- getArgs
    lookupEnv "GITHUB_TOKEN" >>= \case
        Nothing -> fail "GITHUB_TOKEN environment variable must be set"
        Just (GitHub.OAuth . BS8.pack -> token) ->
            case args of
                [file] -> do
                    yaml <- decodeFileThrow file
                    syncSettings token yaml
                _      -> fail "Usage: hub-settings <settings.yml>"
