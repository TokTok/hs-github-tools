{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.Installation where

import           Control.Applicative       ((<$>), (<*>))
import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Installation

data Installation = Installation
    { installationId     :: Int
    , installationNodeId :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Installation where
    parseJSON (Object x) = Installation
        <$> x .: "id"
        <*> x .: "node_id"

    parseJSON _ = fail "Installation"


instance ToJSON Installation where
    toJSON Installation{..} = object
        [ "id"      .= installationId
        , "node_id" .= installationNodeId
        ]


instance Arbitrary Installation where
    arbitrary = Installation
        <$> arbitrary
        <*> arbitrary
