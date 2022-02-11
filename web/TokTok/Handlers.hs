{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module TokTok.Handlers
  ( handleEvent
  ) where

import qualified Data.ByteString.Char8  as BS8
import           Data.Maybe             (fromMaybe)
import           GitHub.Data.Name       (Name (..))
import           GitHub.Tools.AutoMerge (autoMergeRepo, trustedAuthors)
import           GitHub.Types           (Author (..), CheckCommit (..),
                                         CheckSuite (..), CheckSuiteEvent (..),
                                         Payload (..), Repository (..))
import           System.Environment     (getEnv)


handleEvent :: Payload -> IO ()
handleEvent (CheckSuiteEventPayload (CheckSuiteEvent
    { checkSuiteEventCheckSuite = CheckSuite
        { checkSuiteConclusion = Just "success"
        , checkSuiteHeadBranch = branch
        , checkSuiteHeadCommit = CheckCommit
            { checkCommitCommitter = Author{ authorName = author }
            }
        }
    , checkSuiteEventRepository = Repository{ repositoryName = repo }
    })) | branch /= Just "master" && author `elem` trustedAuthors = do
      -- Get auth token from the $GITHUB_TOKEN environment variable.
      token <- getEnv "GITHUB_TOKEN"
      autoMergePullRequest token "TokTok" (N repo) author

handleEvent _ = return ()
