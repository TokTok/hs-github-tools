{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.PageBuildEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data PageBuildEvent = PageBuildEvent
    { pageBuildEventInstallation :: Maybe Installation
    , pageBuildEventOrganization :: Organization
    , pageBuildEventRepository   :: Repository
    , pageBuildEventSender       :: User

    , pageBuildEventId           :: Int
    , pageBuildEventBuild        :: PageBuild
    } deriving (Eq, Show, Read)

instance Event PageBuildEvent where
    typeName = TypeName "PageBuildEvent"
    eventName = EventName "page_build"

instance FromJSON PageBuildEvent where
    parseJSON (Object x) = PageBuildEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "id"
        <*> x .: "build"

    parseJSON _ = fail "PageBuildEvent"

instance ToJSON PageBuildEvent where
    toJSON PageBuildEvent{..} = object
        [ "installation" .= pageBuildEventInstallation
        , "organization" .= pageBuildEventOrganization
        , "repository"   .= pageBuildEventRepository
        , "sender"       .= pageBuildEventSender

        , "id"           .= pageBuildEventId
        , "build"        .= pageBuildEventBuild
        ]


instance Arbitrary PageBuildEvent where
    arbitrary = PageBuildEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
