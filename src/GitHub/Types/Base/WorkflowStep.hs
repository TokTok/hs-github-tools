{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.WorkflowStep where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- WorkflowStep

data WorkflowStep = WorkflowStep
    { workflowStepName        :: Text
    , workflowStepStatus      :: Text
    , workflowStepConclusion  :: Maybe Text
    , workflowStepNumber      :: Int
    , workflowStepStartedAt   :: Maybe Text
    , workflowStepCompletedAt :: Maybe Text
    } deriving (Eq, Show, Read)


instance FromJSON WorkflowStep where
    parseJSON (Object x) = WorkflowStep
        <$> x .: "name"
        <*> x .: "status"
        <*> x .: "conclusion"
        <*> x .: "number"
        <*> x .: "started_at"
        <*> x .: "completed_at"

    parseJSON _ = fail "WorkflowStep"


instance ToJSON WorkflowStep where
    toJSON WorkflowStep{..} = object
        [ "name"         .= workflowStepName
        , "status"       .= workflowStepStatus
        , "conclusion"   .= workflowStepConclusion
        , "number"       .= workflowStepNumber
        , "started_at"   .= workflowStepStartedAt
        , "completed_at" .= workflowStepCompletedAt
        ]


instance Arbitrary WorkflowStep where
    arbitrary = WorkflowStep
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
