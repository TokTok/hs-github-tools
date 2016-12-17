{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.WatchEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

import           GitHub.Types.Base



data WatchEvent = WatchEvent
    { watchEventOrganization :: Organization
    , watchEventRepository   :: Repository
    , watchEventSender       :: User

    , watchEventAction       :: Text
    } deriving (Eq, Show, Read)

instance FromJSON WatchEvent where
    parseJSON (Object x) = WatchEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"

    parseJSON _ = fail "WatchEvent"

instance ToJSON WatchEvent where
    toJSON WatchEvent{..} = object
        [ "organization" .= watchEventOrganization
        , "repository"   .= watchEventRepository
        , "sender"       .= watchEventSender

        , "action"       .= watchEventAction
        ]
