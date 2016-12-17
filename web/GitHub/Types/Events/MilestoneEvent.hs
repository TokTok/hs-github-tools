{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.MilestoneEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

import           GitHub.Types.Base


data MilestoneEvent = MilestoneEvent
    { milestoneEventOrganization :: Organization
    , milestoneEventRepository   :: Repository
    , milestoneEventSender       :: User

    , milestoneEventAction       :: Text
    , milestoneEventMilestone    :: Milestone
    } deriving (Eq, Show, Read)

instance FromJSON MilestoneEvent where
    parseJSON (Object x) = MilestoneEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "milestone"

    parseJSON _ = fail "MilestoneEvent"

instance ToJSON MilestoneEvent where
    toJSON MilestoneEvent{..} = object
        [ "organization" .= milestoneEventOrganization
        , "repository"   .= milestoneEventRepository
        , "sender"       .= milestoneEventSender

        , "action"       .= milestoneEventAction
        , "milestone"    .= milestoneEventMilestone
        ]
