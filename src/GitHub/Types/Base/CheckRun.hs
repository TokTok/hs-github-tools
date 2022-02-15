{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.CheckRun where

import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     object)
import           Data.Aeson.Types                   (Value (..), (.:), (.=))
import           Data.Text                          (Text)
import           Test.QuickCheck.Arbitrary          (Arbitrary (..))

import           GitHub.Types.Base.CheckApp
import           GitHub.Types.Base.CheckOutput
import           GitHub.Types.Base.CheckPullRequest
import           GitHub.Types.Base.CheckSuite

------------------------------------------------------------------------------
-- CheckRun

data CheckRun = CheckRun
    { checkRunApp          :: CheckApp
    , checkRunCheckSuite   :: CheckSuite
    , checkRunCompletedAt  :: Maybe Text
    , checkRunConclusion   :: Maybe Text
    , checkRunDetailsUrl   :: Text
    , checkRunExternalId   :: Text
    , checkRunHeadSha      :: Text
    , checkRunHtmlUrl      :: Text
    , checkRunId           :: Int
    , checkRunName         :: Text
    , checkRunNodeId       :: Text
    , checkRunOutput       :: CheckOutput
    , checkRunPullRequests :: [CheckPullRequest]
    , checkRunStartedAt    :: Text
    , checkRunStatus       :: Text
    , checkRunUrl          :: Text
    } deriving (Eq, Show, Read)


instance FromJSON CheckRun where
    parseJSON (Object x) = CheckRun
        <$> x .: "app"
        <*> x .: "check_suite"
        <*> x .: "completed_at"
        <*> x .: "conclusion"
        <*> x .: "details_url"
        <*> x .: "external_id"
        <*> x .: "head_sha"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "name"
        <*> x .: "node_id"
        <*> x .: "output"
        <*> x .: "pull_requests"
        <*> x .: "started_at"
        <*> x .: "status"
        <*> x .: "url"

    parseJSON _ = fail "CheckRun"


instance ToJSON CheckRun where
    toJSON CheckRun{..} = object
        [ "app"           .= checkRunApp
        , "check_suite"   .= checkRunCheckSuite
        , "completed_at"  .= checkRunCompletedAt
        , "conclusion"    .= checkRunConclusion
        , "details_url"   .= checkRunDetailsUrl
        , "external_id"   .= checkRunExternalId
        , "head_sha"      .= checkRunHeadSha
        , "html_url"      .= checkRunHtmlUrl
        , "id"            .= checkRunId
        , "name"          .= checkRunName
        , "node_id"       .= checkRunNodeId
        , "output"        .= checkRunOutput
        , "pull_requests" .= checkRunPullRequests
        , "started_at"    .= checkRunStartedAt
        , "status"        .= checkRunStatus
        , "url"           .= checkRunUrl
        ]


instance Arbitrary CheckRun where
    arbitrary = CheckRun
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
