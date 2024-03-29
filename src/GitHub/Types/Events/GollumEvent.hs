{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.GollumEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data GollumEvent = GollumEvent
    { gollumEventInstallation :: Maybe Installation
    , gollumEventOrganization :: Organization
    , gollumEventRepository   :: Repository
    , gollumEventSender       :: User

    , gollumEventPages        :: Text
    } deriving (Eq, Show, Read)

instance Event GollumEvent where
    typeName = TypeName "GollumEvent"
    eventName = EventName "gollum"

instance FromJSON GollumEvent where
    parseJSON (Object x) = GollumEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "pages"

    parseJSON _ = fail "GollumEvent"

instance ToJSON GollumEvent where
    toJSON GollumEvent{..} = object
        [ "installation" .= gollumEventInstallation
        , "organization" .= gollumEventOrganization
        , "repository"   .= gollumEventRepository
        , "sender"       .= gollumEventSender

        , "pages"      .= gollumEventPages
        ]


instance Arbitrary GollumEvent where
    arbitrary = GollumEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
