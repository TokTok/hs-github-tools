{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.MilestoneEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data MilestoneEvent = MilestoneEvent
    { milestoneEventInstallation :: Maybe Installation
    , milestoneEventOrganization :: Organization
    , milestoneEventRepository   :: Repository
    , milestoneEventSender       :: User

    , milestoneEventAction       :: Text
    , milestoneEventMilestone    :: Milestone
    } deriving (Eq, Show, Read)

instance Event MilestoneEvent where
    typeName = TypeName "MilestoneEvent"
    eventName = EventName "milestone"

instance FromJSON MilestoneEvent where
    parseJSON (Object x) = MilestoneEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "milestone"

    parseJSON _ = fail "MilestoneEvent"

instance ToJSON MilestoneEvent where
    toJSON MilestoneEvent{..} = object
        [ "installation" .= milestoneEventInstallation
        , "organization" .= milestoneEventOrganization
        , "repository"   .= milestoneEventRepository
        , "sender"       .= milestoneEventSender

        , "action"       .= milestoneEventAction
        , "milestone"    .= milestoneEventMilestone
        ]


instance Arbitrary MilestoneEvent where
    arbitrary = MilestoneEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
