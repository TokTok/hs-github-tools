{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.StatusEvent where

import           Control.Applicative       ((<$>), (<*>))
import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data StatusEvent = StatusEvent
    { statusEventInstallation :: Maybe Installation
    , statusEventOrganization :: Organization
    , statusEventRepository   :: Repository
    , statusEventSender       :: User

    , statusEventAvatarUrl    :: Maybe Text
    , statusEventBranches     :: [Branch]
    , statusEventCommit       :: StatusCommit
    , statusEventContext      :: Text
    , statusEventCreatedAt    :: DateTime
    , statusEventDescription  :: Text
    , statusEventId           :: Int
    , statusEventName         :: Text
    , statusEventSha          :: Text
    , statusEventState        :: Text
    , statusEventTargetUrl    :: Maybe Text
    , statusEventUpdatedAt    :: DateTime
    } deriving (Eq, Show, Read)

instance Event StatusEvent where
    typeName = TypeName "StatusEvent"
    eventName = EventName "status"

instance FromJSON StatusEvent where
    parseJSON (Object x) = StatusEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "avatar_url"
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
        [ "installation" .= statusEventInstallation
        , "organization" .= statusEventOrganization
        , "repository"   .= statusEventRepository
        , "sender"       .= statusEventSender

        , "avatar_url"   .= statusEventAvatarUrl
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


instance Arbitrary StatusEvent where
    arbitrary = StatusEvent
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
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
