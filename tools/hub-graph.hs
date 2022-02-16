{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8     as BS8
import qualified GitHub
import           System.Environment        (lookupEnv)

import           GitHub.Tools.NetworkGraph


repos :: [(GitHub.Name GitHub.Owner, GitHub.Name GitHub.Repo)]
repos =
    [ ("TokTok", "c-toxcore")
    , ("irungentoo", "toxcore")
    ]

main :: IO ()
main = do
  -- Get auth token from the $GITHUB_TOKEN environment variable.
  auth <- fmap (GitHub.OAuth . BS8.pack) <$> lookupEnv "GITHUB_TOKEN"
  putStr =<< getNetworkGraph auth repos
