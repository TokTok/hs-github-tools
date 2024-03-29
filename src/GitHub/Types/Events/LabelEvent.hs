{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.LabelEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data LabelEvent = LabelEvent
    { labelEventInstallation :: Maybe Installation
    , labelEventOrganization :: Organization
    , labelEventRepository   :: Repository
    , labelEventSender       :: User

    , labelEventAction       :: Text
    , labelEventLabel        :: Label
    } deriving (Eq, Show, Read)

instance Event LabelEvent where
    typeName = TypeName "LabelEvent"
    eventName = EventName "label"

instance FromJSON LabelEvent where
    parseJSON (Object x) = LabelEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "label"

    parseJSON _ = fail "LabelEvent"

instance ToJSON LabelEvent where
    toJSON LabelEvent{..} = object
        [ "installation"     .= labelEventInstallation
        , "organization"     .= labelEventOrganization
        , "repository"       .= labelEventRepository
        , "sender"           .= labelEventSender

        , "action"           .= labelEventAction
        , "label"            .= labelEventLabel
        ]


instance Arbitrary LabelEvent where
    arbitrary = LabelEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
