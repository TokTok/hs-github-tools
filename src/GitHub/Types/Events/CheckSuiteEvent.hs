{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.CheckSuiteEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data CheckSuiteEvent = CheckSuiteEvent
    { checkSuiteEventInstallation :: Maybe Installation
    , checkSuiteEventOrganization :: Organization
    , checkSuiteEventRepository   :: Repository
    , checkSuiteEventSender       :: User

    , checkSuiteEventAction       :: Text
    , checkSuiteEventCheckSuite   :: CheckSuite
    } deriving (Eq, Show, Read)

instance Event CheckSuiteEvent where
    typeName = TypeName "CheckSuiteEvent"
    eventName = EventName "check_suite"

instance FromJSON CheckSuiteEvent where
    parseJSON (Object x) = CheckSuiteEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "check_suite"

    parseJSON _ = fail "CheckSuiteEvent"

instance ToJSON CheckSuiteEvent where
    toJSON CheckSuiteEvent{..} = object
        [ "installation" .= checkSuiteEventInstallation
        , "organization" .= checkSuiteEventOrganization
        , "repository"   .= checkSuiteEventRepository
        , "sender"       .= checkSuiteEventSender

        , "action"       .= checkSuiteEventAction
        , "check_suite"  .= checkSuiteEventCheckSuite
        ]


instance Arbitrary CheckSuiteEvent where
    arbitrary = CheckSuiteEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
