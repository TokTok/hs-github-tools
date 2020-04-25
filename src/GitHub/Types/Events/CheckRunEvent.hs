{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.CheckRunEvent where

import           Control.Applicative       ((<$>), (<*>))
import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data CheckRunEvent = CheckRunEvent
    { checkRunEventInstallation :: Maybe Installation
    , checkRunEventOrganization :: Organization
    , checkRunEventRepository   :: Repository
    , checkRunEventSender       :: User

    , checkRunEventAction       :: Text
    , checkRunEventCheckRun     :: CheckRun
    } deriving (Eq, Show, Read)

instance Event CheckRunEvent where
    typeName = TypeName "CheckRunEvent"
    eventName = EventName "check_run"

instance FromJSON CheckRunEvent where
    parseJSON (Object x) = CheckRunEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "check_run"

    parseJSON _ = fail "CheckRunEvent"

instance ToJSON CheckRunEvent where
    toJSON CheckRunEvent{..} = object
        [ "installation" .= checkRunEventInstallation
        , "organization" .= checkRunEventOrganization
        , "repository"   .= checkRunEventRepository
        , "sender"       .= checkRunEventSender

        , "action"       .= checkRunEventAction
        , "check_run"    .= checkRunEventCheckRun
        ]


instance Arbitrary CheckRunEvent where
    arbitrary = CheckRunEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
