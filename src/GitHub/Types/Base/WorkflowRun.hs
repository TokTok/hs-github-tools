{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.WorkflowRun where

import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     object)
import           Data.Aeson.Types                   (Value (..), (.:), (.=))
import           Data.Text                          (Text)
import           Test.QuickCheck.Arbitrary          (Arbitrary (..))

import           GitHub.Types.Base.CheckCommit
import           GitHub.Types.Base.CheckPullRequest
import           GitHub.Types.Base.SimpleRepository

------------------------------------------------------------------------------
-- WorkflowRun

data WorkflowRun = WorkflowRun
    { workflowRunConclusion         :: Maybe Text
    , workflowRunRepository         :: SimpleRepository
    , workflowRunHeadRepository     :: SimpleRepository
    , workflowRunHeadBranch         :: Text
    , workflowRunHeadCommit         :: CheckCommit
    , workflowRunHeadSha            :: Text
    , workflowRunId                 :: Int
    , workflowRunWorkflowId         :: Int
    , workflowRunNodeId             :: Text
    , workflowRunCheckSuiteId       :: Int
    , workflowRunCheckSuiteNodeId   :: Text
    , workflowRunPullRequests       :: [CheckPullRequest]
    , workflowRunStatus             :: Text
    , workflowRunUrl                :: Text
    , workflowRunJobsUrl            :: Text
    , workflowRunLogsUrl            :: Text
    , workflowRunCheckSuiteUrl      :: Text
    , workflowRunArtifactsUrl       :: Text
    , workflowRunCancelUrl          :: Text
    , workflowRunPreviousAttemptUrl :: Maybe Text
    , workflowRunWorkflowUrl        :: Text
    , workflowRunRerunUrl           :: Text
    , workflowRunHtmlUrl            :: Text
    , workflowRunRunStartedAt       :: Text
    , workflowRunUpdatedAt          :: Text
    , workflowRunCreatedAt          :: Text
    , workflowRunRunNumber          :: Int
    , workflowRunRunAttempt         :: Int
    , workflowRunEvent              :: Text
    , workflowRunName               :: Text
    } deriving (Eq, Show, Read)


instance FromJSON WorkflowRun where
    parseJSON (Object x) = WorkflowRun
        <$> x .: "conclusion"
        <*> x .: "repository"
        <*> x .: "head_repository"
        <*> x .: "head_branch"
        <*> x .: "head_commit"
        <*> x .: "head_sha"
        <*> x .: "id"
        <*> x .: "workflow_id"
        <*> x .: "node_id"
        <*> x .: "check_suite_id"
        <*> x .: "check_suite_node_id"
        <*> x .: "pull_requests"
        <*> x .: "status"
        <*> x .: "url"
        <*> x .: "jobs_url"
        <*> x .: "logs_url"
        <*> x .: "check_suite_url"
        <*> x .: "artifacts_url"
        <*> x .: "cancel_url"
        <*> x .: "previous_attempt_url"
        <*> x .: "workflow_url"
        <*> x .: "rerun_url"
        <*> x .: "html_url"
        <*> x .: "run_started_at"
        <*> x .: "updated_at"
        <*> x .: "created_at"
        <*> x .: "run_number"
        <*> x .: "run_attempt"
        <*> x .: "event"
        <*> x .: "name"

    parseJSON _ = fail "WorkflowRun"


instance ToJSON WorkflowRun where
    toJSON WorkflowRun{..} = object
        [ "conclusion"              .= workflowRunConclusion
        , "repository"              .= workflowRunRepository
        , "head_repository"         .= workflowRunHeadRepository
        , "head_branch"             .= workflowRunHeadBranch
        , "head_commit"             .= workflowRunHeadCommit
        , "head_sha"                .= workflowRunHeadSha
        , "id"                      .= workflowRunId
        , "workflow_id"             .= workflowRunWorkflowId
        , "node_id"                 .= workflowRunNodeId
        , "check_suite_id"          .= workflowRunCheckSuiteId
        , "check_suite_node_id"     .= workflowRunCheckSuiteNodeId
        , "pull_requests"           .= workflowRunPullRequests
        , "status"                  .= workflowRunStatus
        , "url"                     .= workflowRunUrl
        , "jobs_url"                .= workflowRunJobsUrl
        , "logs_url"                .= workflowRunLogsUrl
        , "check_suite_url"         .= workflowRunCheckSuiteUrl
        , "artifacts_url"           .= workflowRunArtifactsUrl
        , "cancel_url"              .= workflowRunCancelUrl
        , "previous_attempt_url"    .= workflowRunPreviousAttemptUrl
        , "workflow_url"            .= workflowRunWorkflowUrl
        , "rerun_url"               .= workflowRunRerunUrl
        , "html_url"                .= workflowRunHtmlUrl
        , "run_started_at"          .= workflowRunRunStartedAt
        , "updated_at"              .= workflowRunUpdatedAt
        , "created_at"              .= workflowRunCreatedAt
        , "run_number"              .= workflowRunRunNumber
        , "run_attempt"             .= workflowRunRunAttempt
        , "event"                   .= workflowRunEvent
        , "name"                    .= workflowRunName
        ]


instance Arbitrary WorkflowRun where
    arbitrary = WorkflowRun
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
