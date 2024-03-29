{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.License where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- License

data License = License
    { licenseKey    :: Text
    , licenseName   :: Text
    , licenseNodeId :: Text
    , licenseSpdxId :: Maybe Text
    , licenseUrl    :: Maybe Text
    } deriving (Eq, Show, Read)


instance FromJSON License where
    parseJSON (Object x) = License
        <$> x .: "key"
        <*> x .: "name"
        <*> x .: "node_id"
        <*> x .: "spdx_id"
        <*> x .: "url"

    parseJSON _ = fail "License"


instance ToJSON License where
    toJSON License{..} = object
        [ "key"     .= licenseKey
        , "name"    .= licenseName
        , "node_id" .= licenseNodeId
        , "spdx_id" .= licenseSpdxId
        , "url"     .= licenseUrl
        ]

instance Arbitrary License where
    arbitrary = License
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
