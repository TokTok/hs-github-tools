{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.RepositoryEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data RepositoryEvent = RepositoryEvent
    { repositoryEventInstallation :: Maybe Installation
    , repositoryEventOrganization :: Organization
    , repositoryEventRepository   :: Repository
    , repositoryEventSender       :: User

    , repositoryEventAction       :: Text
    } deriving (Eq, Show, Read)

instance Event RepositoryEvent where
    typeName = TypeName "RepositoryEvent"
    eventName = EventName "repository"

instance FromJSON RepositoryEvent where
    parseJSON (Object x) = RepositoryEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"

    parseJSON _ = fail "RepositoryEvent"

instance ToJSON RepositoryEvent where
    toJSON RepositoryEvent{..} = object
        [ "installation"     .= repositoryEventInstallation
        , "organization"     .= repositoryEventOrganization
        , "repository"       .= repositoryEventRepository
        , "sender"           .= repositoryEventSender

        , "action"           .= repositoryEventAction
        ]


instance Arbitrary RepositoryEvent where
    arbitrary = RepositoryEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
