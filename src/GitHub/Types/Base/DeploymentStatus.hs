{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.DeploymentStatus where

import           Data.Aeson                 (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types           (Value (..), (.:), (.=))
import           Data.Text                  (Text)
import           Test.QuickCheck.Arbitrary  (Arbitrary (..))

import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- DeploymentStatus

data DeploymentStatus = DeploymentStatus
    { deploymentStatusUrl           :: Text
    , deploymentStatusId            :: Int
    , deploymentStatusState         :: Text
    , deploymentStatusNodeId        :: Text
    , deploymentStatusCreator       :: User
    , deploymentStatusDescription   :: Text
    , deploymentStatusEnvironment   :: Text
    , deploymentStatusTargetUrl     :: Text
    , deploymentStatusLogUrl        :: Text
    , deploymentStatusCreatedAt     :: DateTime
    , deploymentStatusUpdatedAt     :: DateTime
    , deploymentStatusDeploymentUrl :: Text
    , deploymentStatusRepositoryUrl :: Text
    } deriving (Eq, Show, Read)

instance FromJSON DeploymentStatus where
    parseJSON (Object x) = DeploymentStatus
        <$> x .: "url"
        <*> x .: "id"
        <*> x .: "state"
        <*> x .: "node_id"
        <*> x .: "creator"
        <*> x .: "description"
        <*> x .: "environment"
        <*> x .: "target_url"
        <*> x .: "log_url"
        <*> x .: "created_at"
        <*> x .: "updated_at"
        <*> x .: "deployment_url"
        <*> x .: "repository_url"

    parseJSON _ = fail "DeploymentStatus"

instance ToJSON DeploymentStatus where
    toJSON DeploymentStatus{..} = object
        [ "url"            .= deploymentStatusUrl
        , "id"             .= deploymentStatusId
        , "state"          .= deploymentStatusState
        , "node_id"        .= deploymentStatusNodeId
        , "creator"        .= deploymentStatusCreator
        , "description"    .= deploymentStatusDescription
        , "environment"    .= deploymentStatusEnvironment
        , "target_url"     .= deploymentStatusTargetUrl
        , "log_url"        .= deploymentStatusLogUrl
        , "created_at"     .= deploymentStatusCreatedAt
        , "updated_at"     .= deploymentStatusUpdatedAt
        , "deployment_url" .= deploymentStatusDeploymentUrl
        , "repository_url" .= deploymentStatusRepositoryUrl
        ]


instance Arbitrary DeploymentStatus where
    arbitrary = DeploymentStatus
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
