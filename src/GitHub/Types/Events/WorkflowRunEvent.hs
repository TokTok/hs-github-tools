{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.WorkflowRunEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data WorkflowRunEvent = WorkflowRunEvent
    { workflowRunEventInstallation :: Maybe Installation
    , workflowRunEventOrganization :: Organization
    , workflowRunEventRepository   :: Repository
    , workflowRunEventSender       :: User

    , workflowRunEventAction       :: Text
    , workflowRunEventWorkflow     :: Workflow
    , workflowRunEventWorkflowRun  :: WorkflowRun
    } deriving (Eq, Show, Read)

instance Event WorkflowRunEvent where
    typeName = TypeName "WorkflowRunEvent"
    eventName = EventName "workflow_run"

instance FromJSON WorkflowRunEvent where
    parseJSON (Object x) = WorkflowRunEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "workflow"
        <*> x .: "workflow_run"

    parseJSON _ = fail "WorkflowRunEvent"

instance ToJSON WorkflowRunEvent where
    toJSON WorkflowRunEvent{..} = object
        [ "installation" .= workflowRunEventInstallation
        , "organization" .= workflowRunEventOrganization
        , "repository"   .= workflowRunEventRepository
        , "sender"       .= workflowRunEventSender

        , "action"       .= workflowRunEventAction
        , "workflow"     .= workflowRunEventWorkflow
        , "workflow_run" .= workflowRunEventWorkflowRun
        ]


instance Arbitrary WorkflowRunEvent where
    arbitrary = WorkflowRunEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
