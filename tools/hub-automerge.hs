{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8  as BS8
import qualified GitHub
import           System.Environment     (getEnv)

import           GitHub.Tools.AutoMerge (autoMergeAll)


main :: IO ()
main = do
    -- Get auth token from the $GITHUB_TOKEN environment variable.
    token <- getEnv "GITHUB_TOKEN"
    let auth = GitHub.OAuth . BS8.pack $ token
    autoMergeAll "TokTok" "TokTok" token auth
