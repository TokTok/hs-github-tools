{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module TokTok.Handlers
  ( handleEvent
  ) where

import           Data.Text              (Text)
import           GitHub.Data.Name       (Name (..))
import           GitHub.Tools.AutoMerge (autoMergePullRequest, trustedAuthors)
import           GitHub.Types           (Author (..), CheckCommit (..),
                                         CheckSuite (..), CheckSuiteEvent (..),
                                         Payload (..), Repository (..))
import           System.Environment     (getEnv)


handleEvent :: Payload -> Maybe (Text, IO ())
handleEvent (CheckSuiteEventPayload (CheckSuiteEvent
    { checkSuiteEventCheckSuite = CheckSuite
        { checkSuiteConclusion = Just "success"
        , checkSuiteHeadBranch = branch
        , checkSuiteHeadCommit = Just (CheckCommit
            { checkCommitCommitter = Author{ authorName = author }
            })
        }
    , checkSuiteEventRepository = Repository{ repositoryName = repo }
    })) | branch /= Just "master" && author `elem` trustedAuthors =
        Just (repo <> "/" <> author, do
            -- Get auth token from the $GITHUB_TOKEN environment variable.
            token <- getEnv "GITHUB_TOKEN"
            autoMergePullRequest token "TokTok" (N repo) author)

handleEvent _ = Nothing
