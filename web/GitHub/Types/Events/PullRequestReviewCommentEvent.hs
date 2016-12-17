{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.PullRequestReviewCommentEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

import           GitHub.Types.Base


data PullRequestReviewCommentEvent = PullRequestReviewCommentEvent
    { pullRequestReviewCommentEventOrganization :: Organization
    , pullRequestReviewCommentEventRepository   :: Repository
    , pullRequestReviewCommentEventSender       :: User

    , pullRequestReviewCommentEventAction       :: Text
    , pullRequestReviewCommentEventComment      :: ReviewComment
    , pullRequestReviewCommentEventPullRequest  :: SimplePullRequest
    } deriving (Eq, Show, Read)

instance FromJSON PullRequestReviewCommentEvent where
    parseJSON (Object x) = PullRequestReviewCommentEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "comment"
        <*> x .: "pull_request"

    parseJSON _ = fail "PullRequestReviewCommentEvent"

instance ToJSON PullRequestReviewCommentEvent where
    toJSON PullRequestReviewCommentEvent{..} = object
        [ "organization" .= pullRequestReviewCommentEventOrganization
        , "repository"   .= pullRequestReviewCommentEventRepository
        , "sender"       .= pullRequestReviewCommentEventSender

        , "action"       .= pullRequestReviewCommentEventAction
        , "comment"      .= pullRequestReviewCommentEventComment
        , "pull_request" .= pullRequestReviewCommentEventPullRequest
        ]
