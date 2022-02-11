{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module TokTok.Handlers
  ( handleEvent
  ) where

import qualified Data.ByteString.Char8  as BS8
import           Data.Maybe             (fromMaybe)
import qualified GitHub
import           GitHub.Data.Name       (Name (..))
import           GitHub.Tools.AutoMerge (autoMergeRepo)
import           GitHub.Types           (CheckSuite (..), CheckSuiteEvent (..),
                                         Payload (..), Repository (..))
import           System.Environment     (getEnv)


handleEvent :: Payload -> IO ()
handleEvent (CheckSuiteEventPayload event)
  | ("success" ==) . fromMaybe "" . checkSuiteConclusion . checkSuiteEventCheckSuite $ event = do
      -- Get auth token from the $GITHUB_TOKEN environment variable.
      token <- getEnv "GITHUB_TOKEN"
      let auth = GitHub.OAuth . BS8.pack $ token
      let repo = repositoryName . checkSuiteEventRepository $ event
      autoMergeRepo "TokTok" "TokTok" (N repo) token auth

handleEvent _ = return ()
