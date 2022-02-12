{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module TokTok.Handlers
  ( handleEvent
  ) where

import           Data.Text              (Text)
import           GitHub.Data.Name       (Name (..))
import           GitHub.Tools.AutoMerge (autoMergePullRequest)
import           GitHub.Types           (CheckSuite (..), CheckSuiteEvent (..),
                                         Payload (..), Repository (..))
import           System.Environment     (getEnv)


handleEvent :: Payload -> Maybe (Text, IO ())
handleEvent (CheckSuiteEventPayload (CheckSuiteEvent
    { checkSuiteEventCheckSuite = CheckSuite
        { checkSuiteConclusion = Just "success"
        , checkSuiteHeadBranch = branch
        }
    , checkSuiteEventRepository = Repository{ repositoryName = repo }
    })) | branch /= Just "master" =
        Just (repo, do
            -- Get auth token from the $GITHUB_TOKEN environment variable.
            token <- getEnv "GITHUB_TOKEN"
            autoMergePullRequest token "TokTok" (N repo))

handleEvent _ = Nothing
