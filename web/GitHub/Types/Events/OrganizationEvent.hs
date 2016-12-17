{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.OrganizationEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

import           GitHub.Types.Base


data OrganizationEvent = OrganizationEvent
    { organizationEventOrganization :: Organization
    , organizationEventSender       :: User

    , organizationEventAction       :: Text
    , organizationEventMembership   :: Membership
    } deriving (Eq, Show, Read)

instance FromJSON OrganizationEvent where
    parseJSON (Object x) = OrganizationEvent
        <$> x .: "organization"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "membership"

    parseJSON _ = fail "OrganizationEvent"

instance ToJSON OrganizationEvent where
    toJSON OrganizationEvent{..} = object
        [ "organization" .= organizationEventOrganization
        , "sender"       .= organizationEventSender

        , "action"       .= organizationEventAction
        , "membership"   .= organizationEventMembership
        ]
