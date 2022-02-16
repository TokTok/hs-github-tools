{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Deployment where

import           Data.Aeson                          (FromJSON (..),
                                                      ToJSON (..), object)
import           Data.Aeson.Types                    (Value (..), (.:), (.=))
import           Data.Text                           (Text)
import           Test.QuickCheck.Arbitrary           (Arbitrary (..))

import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.DeploymentPayload
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- Deployment

data Deployment = Deployment
    { deploymentUrl                   :: Text
    , deploymentId                    :: Int
    , deploymentNodeId                :: Text
    , deploymentSha                   :: Text
    , deploymentRef                   :: Text
    , deploymentTask                  :: Text
    , deploymentPayload               :: Maybe DeploymentPayload
    , deploymentEnvironment           :: Text
    , deploymentOriginalEnvironment   :: Text
    , deploymentProductionEnvironment :: Bool
    , deploymentTransientEnvironment  :: Bool
    , deploymentDescription           :: Maybe Text
    , deploymentCreator               :: User
    , deploymentCreatedAt             :: DateTime
    , deploymentUpdatedAt             :: DateTime
    , deploymentStatusesUrl           :: Text
    , deploymentRepositoryUrl         :: Text
    } deriving (Eq, Show, Read)

instance FromJSON Deployment where
    parseJSON (Object x) = Deployment
        <$> x .: "url"
        <*> x .: "id"
        <*> x .: "node_id"
        <*> x .: "sha"
        <*> x .: "ref"
        <*> x .: "task"
        <*> x .: "payload"
        <*> x .: "environment"
        <*> x .: "original_environment"
        <*> x .: "production_environment"
        <*> x .: "transient_environment"
        <*> x .: "description"
        <*> x .: "creator"
        <*> x .: "created_at"
        <*> x .: "updated_at"
        <*> x .: "statuses_url"
        <*> x .: "repository_url"

    parseJSON _ = fail "Deployment"

instance ToJSON Deployment where
    toJSON Deployment{..} = object
        [ "url"                    .= deploymentUrl
        , "id"                     .= deploymentId
        , "node_id"                .= deploymentNodeId
        , "sha"                    .= deploymentSha
        , "ref"                    .= deploymentRef
        , "task"                   .= deploymentTask
        , "payload"                .= deploymentPayload
        , "environment"            .= deploymentEnvironment
        , "original_environment"   .= deploymentOriginalEnvironment
        , "production_environment" .= deploymentProductionEnvironment
        , "transient_environment"  .= deploymentTransientEnvironment
        , "description"            .= deploymentDescription
        , "creator"                .= deploymentCreator
        , "created_at"             .= deploymentCreatedAt
        , "updated_at"             .= deploymentUpdatedAt
        , "statuses_url"           .= deploymentStatusesUrl
        , "repository_url"         .= deploymentRepositoryUrl
        ]


instance Arbitrary Deployment where
    arbitrary = Deployment
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
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
