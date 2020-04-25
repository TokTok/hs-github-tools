{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.StarEvent where

import           Control.Applicative       ((<$>), (<*>))
import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data StarEvent = StarEvent
    { starEventInstallation :: Maybe Installation
    , starEventOrganization :: Organization
    , starEventRepository   :: Repository
    , starEventSender       :: User

    , starEventAction       :: Text
    , starEventStarredAt    :: Text
    } deriving (Eq, Show, Read)

instance Event StarEvent where
    typeName = TypeName "StarEvent"
    eventName = EventName "star"

instance FromJSON StarEvent where
    parseJSON (Object x) = StarEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "starred_at"

    parseJSON _ = fail "StarEvent"

instance ToJSON StarEvent where
    toJSON StarEvent{..} = object
        [ "installation" .= starEventInstallation
        , "organization" .= starEventOrganization
        , "repository"   .= starEventRepository
        , "sender"       .= starEventSender

        , "action"       .= starEventAction
        , "starred_at"   .= starEventStarredAt
        ]


instance Arbitrary StarEvent where
    arbitrary = StarEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
