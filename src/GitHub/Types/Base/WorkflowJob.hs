{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.WorkflowJob where

import           Data.Aeson                     (FromJSON (..), ToJSON (..),
                                                 object)
import           Data.Aeson.Types               (Value (..), (.:), (.=))
import           Data.Text                      (Text)
import           Test.QuickCheck.Arbitrary      (Arbitrary (..))

import           GitHub.Types.Base.WorkflowStep

------------------------------------------------------------------------------
-- WorkflowJob

data WorkflowJob = WorkflowJob
    { workflowJobConclusion      :: Maybe Text
    , workflowJobHeadSha         :: Text
    , workflowJobRunAttempt      :: Int
    , workflowJobRunId           :: Int
    , workflowJobRunUrl          :: Text
    , workflowJobCheckRunUrl     :: Text
    , workflowJobHtmlUrl         :: Text
    , workflowJobId              :: Int
    , workflowJobNodeId          :: Text
    , workflowJobName            :: Text
    , workflowJobLabels          :: [Text]
    , workflowJobSteps           :: [WorkflowStep]
    , workflowJobStatus          :: Text
    , workflowJobUrl             :: Text
    , workflowJobStartedAt       :: Text
    , workflowJobCompletedAt     :: Maybe Text
    , workflowJobRunnerId        :: Maybe Int
    , workflowJobRunnerName      :: Maybe Text
    , workflowJobRunnerGroupId   :: Maybe Int
    , workflowJobRunnerGroupName :: Maybe Text
    } deriving (Eq, Show, Read)


instance FromJSON WorkflowJob where
    parseJSON (Object x) = WorkflowJob
        <$> x .: "conclusion"
        <*> x .: "head_sha"
        <*> x .: "run_attempt"
        <*> x .: "run_id"
        <*> x .: "run_url"
        <*> x .: "check_run_url"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "node_id"
        <*> x .: "name"
        <*> x .: "labels"
        <*> x .: "steps"
        <*> x .: "status"
        <*> x .: "url"
        <*> x .: "started_at"
        <*> x .: "completed_at"
        <*> x .: "runner_id"
        <*> x .: "runner_name"
        <*> x .: "runner_group_id"
        <*> x .: "runner_group_name"

    parseJSON _ = fail "WorkflowJob"


instance ToJSON WorkflowJob where
    toJSON WorkflowJob{..} = object
        [ "conclusion"        .= workflowJobConclusion
        , "head_sha"          .= workflowJobHeadSha
        , "run_attempt"       .= workflowJobRunAttempt
        , "run_id"            .= workflowJobRunId
        , "run_url"           .= workflowJobRunUrl
        , "check_run_url"     .= workflowJobCheckRunUrl
        , "html_url"          .= workflowJobHtmlUrl
        , "id"                .= workflowJobId
        , "node_id"           .= workflowJobNodeId
        , "name"              .= workflowJobName
        , "labels"            .= workflowJobLabels
        , "steps"             .= workflowJobSteps
        , "status"            .= workflowJobStatus
        , "url"               .= workflowJobUrl
        , "started_at"        .= workflowJobStartedAt
        , "completed_at"      .= workflowJobCompletedAt
        , "runner_id"         .= workflowJobRunnerId
        , "runner_name"       .= workflowJobRunnerName
        , "runner_group_id"   .= workflowJobRunnerGroupId
        , "runner_group_name" .= workflowJobRunnerGroupName
        ]


instance Arbitrary WorkflowJob where
    arbitrary = WorkflowJob
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
        <*> arbitrary
        <*> arbitrary
