{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.WorkflowJobEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data WorkflowJobEvent = WorkflowJobEvent
    { workflowJobEventInstallation :: Maybe Installation
    , workflowJobEventOrganization :: Organization
    , workflowJobEventRepository   :: Repository
    , workflowJobEventSender       :: User

    , workflowJobEventAction       :: Text
    , workflowJobEventWorkflowJob  :: WorkflowJob
    } deriving (Eq, Show, Read)

instance Event WorkflowJobEvent where
    typeName = TypeName "WorkflowJobEvent"
    eventName = EventName "workflow_job"

instance FromJSON WorkflowJobEvent where
    parseJSON (Object x) = WorkflowJobEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "workflow_job"

    parseJSON _ = fail "WorkflowJobEvent"

instance ToJSON WorkflowJobEvent where
    toJSON WorkflowJobEvent{..} = object
        [ "installation" .= workflowJobEventInstallation
        , "organization" .= workflowJobEventOrganization
        , "repository"   .= workflowJobEventRepository
        , "sender"       .= workflowJobEventSender

        , "action"       .= workflowJobEventAction
        , "workflow_job" .= workflowJobEventWorkflowJob
        ]


instance Arbitrary WorkflowJobEvent where
    arbitrary = WorkflowJobEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
