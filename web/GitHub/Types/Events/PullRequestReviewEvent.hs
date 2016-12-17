{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.PullRequestReviewEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

import           GitHub.Types.Base


data PullRequestReviewEvent = PullRequestReviewEvent
    { pullRequestReviewEventOrganization :: Organization
    , pullRequestReviewEventRepository   :: Repository
    , pullRequestReviewEventSender       :: User

    , pullRequestReviewEventAction       :: Text
    , pullRequestReviewEventPullRequest  :: SimplePullRequest
    , pullRequestReviewEventReview       :: Review
    } deriving (Eq, Show, Read)

instance FromJSON PullRequestReviewEvent where
    parseJSON (Object x) = PullRequestReviewEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "pull_request"
        <*> x .: "review"

    parseJSON _ = fail "PullRequestReviewEvent"

instance ToJSON PullRequestReviewEvent where
    toJSON PullRequestReviewEvent{..} = object $
        [ "organization" .= pullRequestReviewEventOrganization
        , "repository"   .= pullRequestReviewEventRepository
        , "sender"       .= pullRequestReviewEventSender

        , "action"       .= pullRequestReviewEventAction
        , "pull_request" .= pullRequestReviewEventPullRequest
        , "review"       .= pullRequestReviewEventReview
        ]
