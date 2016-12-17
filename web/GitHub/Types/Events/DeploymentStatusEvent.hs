{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Events.DeploymentStatusEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))

import           GitHub.Types.Base


data DeploymentStatusEvent = DeploymentStatusEvent
    { deploymentStatusEventOrganization     :: Organization
    , deploymentStatusEventRepository       :: Repository
    , deploymentStatusEventSender           :: User

    , deploymentStatusEventDeployment       :: Deployment
    , deploymentStatusEventDeploymentStatus :: DeploymentStatus
    } deriving (Eq, Show, Read)

instance FromJSON DeploymentStatusEvent where
    parseJSON (Object x) = DeploymentStatusEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "deployment"
        <*> x .: "deployment_status"

    parseJSON _ = fail "DeploymentStatusEvent"

instance ToJSON DeploymentStatusEvent where
    toJSON DeploymentStatusEvent{..} = object
        [ "organization"       .= deploymentStatusEventOrganization
        , "repository"         .= deploymentStatusEventRepository
        , "sender"             .= deploymentStatusEventSender

        , "deployment"         .= deploymentStatusEventDeployment
        , "deployment_status"  .= deploymentStatusEventDeploymentStatus
        ]
