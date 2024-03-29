{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.WatchEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data WatchEvent = WatchEvent
    { watchEventInstallation :: Maybe Installation
    , watchEventOrganization :: Organization
    , watchEventRepository   :: Repository
    , watchEventSender       :: User

    , watchEventAction       :: Text
    } deriving (Eq, Show, Read)

instance Event WatchEvent where
    typeName = TypeName "WatchEvent"
    eventName = EventName "watch"

instance FromJSON WatchEvent where
    parseJSON (Object x) = WatchEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"

    parseJSON _ = fail "WatchEvent"

instance ToJSON WatchEvent where
    toJSON WatchEvent{..} = object
        [ "installation" .= watchEventInstallation
        , "organization" .= watchEventOrganization
        , "repository"   .= watchEventRepository
        , "sender"       .= watchEventSender

        , "action"       .= watchEventAction
        ]


instance Arbitrary WatchEvent where
    arbitrary = WatchEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
