{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.IssuesEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data IssuesEvent = IssuesEvent
    { issuesEventInstallation :: Maybe Installation
    , issuesEventOrganization :: Organization
    , issuesEventRepository   :: Repository
    , issuesEventSender       :: User

    , issuesEventAction       :: Text
    , issuesEventAssignee     :: Maybe User
    , issuesEventChanges      :: Maybe Changes
    , issuesEventIssue        :: Issue
    , issuesEventLabel        :: Maybe Label
    , issuesEventMilestone    :: Maybe Milestone
    } deriving (Eq, Show, Read)

instance Event IssuesEvent where
    typeName = TypeName "IssuesEvent"
    eventName = EventName "issues"

instance FromJSON IssuesEvent where
    parseJSON (Object x) = IssuesEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .:? "assignee"
        <*> x .:? "changes"
        <*> x .: "issue"
        <*> x .:? "label"
        <*> x .:? "milestone"

    parseJSON _ = fail "IssuesEvent"

instance ToJSON IssuesEvent where
    toJSON IssuesEvent{..} = object
        [ "installation" .= issuesEventInstallation
        , "organization" .= issuesEventOrganization
        , "repository"   .= issuesEventRepository
        , "sender"       .= issuesEventSender

        , "action"       .= issuesEventAction
        , "assignee"     .= issuesEventAssignee
        , "changes"      .= issuesEventChanges
        , "issue"        .= issuesEventIssue
        , "label"        .= issuesEventLabel
        , "milestone"    .= issuesEventMilestone
        ]


instance Arbitrary IssuesEvent where
    arbitrary = IssuesEvent
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
