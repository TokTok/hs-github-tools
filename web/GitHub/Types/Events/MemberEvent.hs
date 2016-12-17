{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.MemberEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

import           GitHub.Types.Base


data MemberEvent = MemberEvent
    { memberEventOrganization :: Organization
    , memberEventRepository   :: Repository
    , memberEventSender       :: User

    , memberEventAction       :: Text
    } deriving (Eq, Show, Read)

instance FromJSON MemberEvent where
    parseJSON (Object x) = MemberEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"

    parseJSON _ = fail "MemberEvent"

instance ToJSON MemberEvent where
    toJSON MemberEvent{..} = object
        [ "organization" .= memberEventOrganization
        , "repository"   .= memberEventRepository
        , "sender"       .= memberEventSender

        , "action"      .= memberEventAction
        ]
