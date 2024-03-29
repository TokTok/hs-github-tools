{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.ForkEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data ForkEvent = ForkEvent
    { forkEventInstallation :: Maybe Installation
    , forkEventOrganization :: Organization
    , forkEventRepository   :: Repository
    , forkEventSender       :: User

    , forkEventForkee       :: Repository
    } deriving (Eq, Show, Read)

instance Event ForkEvent where
    typeName = TypeName "ForkEvent"
    eventName = EventName "fork"

instance FromJSON ForkEvent where
    parseJSON (Object x) = ForkEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "forkee"

    parseJSON _ = fail "ForkEvent"

instance ToJSON ForkEvent where
    toJSON ForkEvent{..} = object
        [ "installation" .= forkEventInstallation
        , "organization" .= forkEventOrganization
        , "repository"   .= forkEventRepository
        , "sender"       .= forkEventSender

        , "forkee"       .= forkEventForkee
        ]


instance Arbitrary ForkEvent where
    arbitrary = ForkEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
