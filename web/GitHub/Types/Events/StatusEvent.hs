{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.StatusEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

import           GitHub.Types.Base


data StatusEvent = StatusEvent
    { statusEventOrganization :: Organization
    , statusEventRepository   :: Repository
    , statusEventSender       :: User

    , statusEventBranches     :: [Branch]
    , statusEventCommit       :: StatusCommit
    , statusEventContext      :: Text
    , statusEventCreatedAt    :: DateTime
    , statusEventDescription  :: Text
    , statusEventId           :: Int
    , statusEventName         :: Text
    , statusEventSha          :: Text
    , statusEventState        :: Text
    , statusEventTargetUrl    :: Text
    , statusEventUpdatedAt    :: DateTime
    } deriving (Eq, Show, Read)

instance FromJSON StatusEvent where
    parseJSON (Object x) = StatusEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "branches"
        <*> x .: "commit"
        <*> x .: "context"
        <*> x .: "created_at"
        <*> x .: "description"
        <*> x .: "id"
        <*> x .: "name"
        <*> x .: "sha"
        <*> x .: "state"
        <*> x .: "target_url"
        <*> x .: "updated_at"

    parseJSON _ = fail "StatusEvent"

instance ToJSON StatusEvent where
    toJSON StatusEvent{..} = object
        [ "organization" .= statusEventOrganization
        , "repository"   .= statusEventRepository
        , "sender"       .= statusEventSender

        , "branches"     .= statusEventBranches
        , "commit"       .= statusEventCommit
        , "context"      .= statusEventContext
        , "created_at"   .= statusEventCreatedAt
        , "description"  .= statusEventDescription
        , "id"           .= statusEventId
        , "name"         .= statusEventName
        , "sha"          .= statusEventSha
        , "state"        .= statusEventState
        , "target_url"   .= statusEventTargetUrl
        , "updated_at"   .= statusEventUpdatedAt
        ]
