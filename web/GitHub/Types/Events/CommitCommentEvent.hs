{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.CommitCommentEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

import           GitHub.Types.Base


data CommitCommentEvent = CommitCommentEvent
    { commitCommentEventOrganization :: Organization
    , commitCommentEventRepository   :: Repository
    , commitCommentEventSender       :: User

    , commitCommentEventAction       :: Text
    , commitCommentEventComment      :: CommitComment
    } deriving (Eq, Show, Read)

instance FromJSON CommitCommentEvent where
    parseJSON (Object x) = CommitCommentEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "comment"

    parseJSON _ = fail "CommitCommentEvent"

instance ToJSON CommitCommentEvent where
    toJSON CommitCommentEvent{..} = object
        [ "organization" .= commitCommentEventOrganization
        , "repository"   .= commitCommentEventRepository
        , "sender"       .= commitCommentEventSender

        , "action"       .= commitCommentEventAction
        , "comment"      .= commitCommentEventComment
        ]
