{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.ForkEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))

import           GitHub.Types.Base


data ForkEvent = ForkEvent
    { forkEventOrganization :: Organization
    , forkEventRepository   :: Repository
    , forkEventSender       :: User

    , forkEventForkee       :: Repository
    } deriving (Eq, Show, Read)

instance FromJSON ForkEvent where
    parseJSON (Object x) = ForkEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "forkee"

    parseJSON _ = fail "ForkEvent"

instance ToJSON ForkEvent where
    toJSON ForkEvent{..} = object
        [ "organization" .= forkEventOrganization
        , "repository"   .= forkEventRepository
        , "sender"       .= forkEventSender

        , "forkee"       .= forkEventForkee
        ]
