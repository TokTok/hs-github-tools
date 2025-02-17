{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.String            (fromString)
import           System.Environment     (getArgs, getEnv)

import           GitHub.Tools.AutoMerge (autoMergeAll, autoMergePullRequest)


main :: IO ()
main = do
    -- Get auth token from the $GITHUB_TOKEN environment variable.
    args <- getArgs
    token <- getEnv "GITHUB_TOKEN"
    case args of
      []     -> autoMergeAll "TokTok" "TokTok" token
      [repo] -> autoMergePullRequest token "TokTok" (fromString repo)
      _      -> error "Usage: hub-automerge [repo] [author]"
