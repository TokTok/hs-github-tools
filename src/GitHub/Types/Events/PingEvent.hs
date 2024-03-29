{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.PingEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data PingEvent = PingEvent
    { pingEventInstallation :: Maybe Installation
    , pingEventOrganization :: Organization
    , pingEventSender       :: User

    , pingEventHook         :: Hook
    , pingEventHookId       :: Int
    , pingEventZen          :: Text
    } deriving (Eq, Show, Read)

instance Event PingEvent where
    typeName = TypeName "PingEvent"
    eventName = EventName "ping"

instance FromJSON PingEvent where
    parseJSON (Object x) = PingEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "sender"

        <*> x .: "hook"
        <*> x .: "hook_id"
        <*> x .: "zen"

    parseJSON _ = fail "PingEvent"

instance ToJSON PingEvent where
    toJSON PingEvent{..} = object
        [ "installation" .= pingEventInstallation
        , "organization" .= pingEventOrganization
        , "sender"       .= pingEventSender

        , "hook"         .= pingEventHook
        , "hook_id"      .= pingEventHookId
        , "zen"          .= pingEventZen
        ]


instance Arbitrary PingEvent where
    arbitrary = PingEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
