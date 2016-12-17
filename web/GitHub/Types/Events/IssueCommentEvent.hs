{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.IssueCommentEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.:?), (.=))
import           Data.Text           (Text)

import           GitHub.Types.Base


data IssueCommentEvent = IssueCommentEvent
    { issueCommentEventOrganization :: Organization
    , issueCommentEventRepository   :: Repository
    , issueCommentEventSender       :: User

    , issueCommentEventAction       :: Text
    , issueCommentEventChanges      :: Maybe Changes
    , issueCommentEventComment      :: IssueComment
    , issueCommentEventIssue        :: Issue
    } deriving (Eq, Show, Read)

instance FromJSON IssueCommentEvent where
    parseJSON (Object x) = IssueCommentEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .:? "changes"
        <*> x .: "comment"
        <*> x .: "issue"

    parseJSON _ = fail "IssueCommentEvent"

instance ToJSON IssueCommentEvent where
    toJSON IssueCommentEvent{..} = object
        [ "organization" .= issueCommentEventOrganization
        , "repository"   .= issueCommentEventRepository
        , "sender"       .= issueCommentEventSender

        , "action"       .= issueCommentEventAction
        , "changes"      .= issueCommentEventChanges
        , "comment"      .= issueCommentEventComment
        , "issue"        .= issueCommentEventIssue
        ]
