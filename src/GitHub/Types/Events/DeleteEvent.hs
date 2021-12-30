{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.DeleteEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data DeleteEvent = DeleteEvent
    { deleteEventInstallation :: Maybe Installation
    , deleteEventOrganization :: Organization
    , deleteEventRepository   :: Repository
    , deleteEventSender       :: User

    , deleteEventPusherType   :: Text
    , deleteEventRef          :: Text
    , deleteEventRefType      :: Text
    } deriving (Eq, Show, Read)

instance Event DeleteEvent where
    typeName = TypeName "DeleteEvent"
    eventName = EventName "delete"

instance FromJSON DeleteEvent where
    parseJSON (Object x) = DeleteEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "pusher_type"
        <*> x .: "ref"
        <*> x .: "ref_type"

    parseJSON _ = fail "DeleteEvent"

instance ToJSON DeleteEvent where
    toJSON DeleteEvent{..} = object
        [ "installation" .= deleteEventInstallation
        , "organization" .= deleteEventOrganization
        , "repository"   .= deleteEventRepository
        , "sender"       .= deleteEventSender

        , "pusher_type"  .= deleteEventPusherType
        , "ref"          .= deleteEventRef
        , "ref_type"     .= deleteEventRefType
        ]


instance Arbitrary DeleteEvent where
    arbitrary = DeleteEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
