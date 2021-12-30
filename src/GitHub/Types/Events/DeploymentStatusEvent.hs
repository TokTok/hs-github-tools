{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.DeploymentStatusEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data DeploymentStatusEvent = DeploymentStatusEvent
    { deploymentStatusEventInstallation     :: Maybe Installation
    , deploymentStatusEventOrganization     :: Organization
    , deploymentStatusEventRepository       :: Repository
    , deploymentStatusEventSender           :: User

    , deploymentStatusEventAction           :: Text
    , deploymentStatusEventDeployment       :: Deployment
    , deploymentStatusEventDeploymentStatus :: DeploymentStatus
    } deriving (Eq, Show, Read)

instance Event DeploymentStatusEvent where
    typeName = TypeName "DeploymentStatusEvent"
    eventName = EventName "deployment_status"

instance FromJSON DeploymentStatusEvent where
    parseJSON (Object x) = DeploymentStatusEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "deployment"
        <*> x .: "deployment_status"

    parseJSON _ = fail "DeploymentStatusEvent"

instance ToJSON DeploymentStatusEvent where
    toJSON DeploymentStatusEvent{..} = object
        [ "installation"       .= deploymentStatusEventInstallation
        , "organization"       .= deploymentStatusEventOrganization
        , "repository"         .= deploymentStatusEventRepository
        , "sender"             .= deploymentStatusEventSender

        , "action"             .= deploymentStatusEventAction
        , "deployment"         .= deploymentStatusEventDeployment
        , "deployment_status"  .= deploymentStatusEventDeploymentStatus
        ]


instance Arbitrary DeploymentStatusEvent where
    arbitrary = DeploymentStatusEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
