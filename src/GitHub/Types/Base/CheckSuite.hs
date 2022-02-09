{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.CheckSuite where

import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                object)
import           Data.Aeson.Types              (Value (..), (.:), (.:?), (.=))
import           Data.Text                     (Text)
import           Test.QuickCheck.Arbitrary     (Arbitrary (..))

import           GitHub.Types.Base.CheckApp
import           GitHub.Types.Base.CheckCommit

------------------------------------------------------------------------------
-- CheckSuite

data CheckSuite = CheckSuite
    { checkSuiteAfter                :: Maybe Text
    , checkSuiteApp                  :: CheckApp
    , checkSuiteBefore               :: Maybe Text
    , checkSuiteCheckRunsUrl         :: Maybe Text
    , checkSuiteConclusion           :: Maybe Text
    , checkSuiteHeadBranch           :: Maybe Text
    , checkSuiteHeadCommit           :: Maybe CheckCommit
    , checkSuiteHeadSha              :: Text
    , checkSuiteId                   :: Int
    , checkSuiteLatestCheckRunsCount :: Maybe Int
    , checkSuiteNodeId               :: Text
    , checkSuitePullRequests         :: [Text]
    , checkSuiteRerequestable        :: Bool
    , checkSuiteRunsRerequestable    :: Bool
    , checkSuiteStatus               :: Text
    , checkSuiteUrl                  :: Text
    , checkSuiteUpdatedAt            :: Text
    , checkSuiteCreatedAt            :: Text
    } deriving (Eq, Show, Read)


instance FromJSON CheckSuite where
    parseJSON (Object x) = CheckSuite
        <$> x .: "after"
        <*> x .: "app"
        <*> x .: "before"
        <*> x .:? "check_runs_url"
        <*> x .: "conclusion"
        <*> x .: "head_branch"
        <*> x .:? "head_commit"
        <*> x .: "head_sha"
        <*> x .: "id"
        <*> x .:? "latest_check_runs_count"
        <*> x .: "node_id"
        <*> x .: "pull_requests"
        <*> x .: "rerequestable"
        <*> x .: "runs_rerequestable"
        <*> x .: "status"
        <*> x .: "url"
        <*> x .: "updated_at"
        <*> x .: "created_at"

    parseJSON _ = fail "CheckSuite"


instance ToJSON CheckSuite where
    toJSON CheckSuite{..} = object
        [ "after"                   .= checkSuiteAfter
        , "app"                     .= checkSuiteApp
        , "before"                  .= checkSuiteBefore
        , "check_runs_url"          .= checkSuiteCheckRunsUrl
        , "conclusion"              .= checkSuiteConclusion
        , "head_branch"             .= checkSuiteHeadBranch
        , "head_commit"             .= checkSuiteHeadCommit
        , "head_sha"                .= checkSuiteHeadSha
        , "id"                      .= checkSuiteId
        , "latest_check_runs_count" .= checkSuiteLatestCheckRunsCount
        , "node_id"                 .= checkSuiteNodeId
        , "pull_requests"           .= checkSuitePullRequests
        , "rerequestable"           .= checkSuiteRerequestable
        , "runs_rerequestable"      .= checkSuiteRunsRerequestable
        , "status"                  .= checkSuiteStatus
        , "url"                     .= checkSuiteUrl
        , "updated_at"              .= checkSuiteUpdatedAt
        , "created_at"              .= checkSuiteCreatedAt
        ]


instance Arbitrary CheckSuite where
    arbitrary = CheckSuite
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
