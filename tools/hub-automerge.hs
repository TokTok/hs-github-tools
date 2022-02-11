{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8  as BS8
import           Data.String            (fromString)
import qualified GitHub
import           System.Environment     (getArgs, getEnv)

import           GitHub.Tools.AutoMerge (autoMergeAll, autoMergeRepo)


main :: IO ()
main = do
    -- Get auth token from the $GITHUB_TOKEN environment variable.
    args <- getArgs
    token <- getEnv "GITHUB_TOKEN"
    let auth = GitHub.OAuth . BS8.pack $ token
    case args of
      []    -> autoMergeAll "TokTok" "TokTok" token auth
      repos -> mapM_ (\repo -> autoMergeRepo "TokTok" "TokTok" (fromString repo) token auth) repos
