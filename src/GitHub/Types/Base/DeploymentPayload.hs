{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.DeploymentPayload where

import           Control.Applicative       ((<$>))
import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- DeploymentPayload

data DeploymentPayload = DeploymentPayload
    { deploymentPayloadWebUrl :: Text
    } deriving (Eq, Show, Read)

instance FromJSON DeploymentPayload where
    parseJSON (Object x) = DeploymentPayload
        <$> x .: "web_url"

    parseJSON _ = fail "DeploymentPayload"

instance ToJSON DeploymentPayload where
    toJSON DeploymentPayload{..} = object
        [ "web_url" .= deploymentPayloadWebUrl
        ]


instance Arbitrary DeploymentPayload where
    arbitrary = DeploymentPayload
        <$> arbitrary
