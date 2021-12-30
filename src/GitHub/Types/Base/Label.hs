{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Label where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Label

data Label = Label
    { labelColor   :: Text
    , labelDefault :: Bool
    , labelUrl     :: Text
    , labelName    :: Text
    , labelId      :: Int
    , labelNodeId  :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Label where
    parseJSON (Object x) = Label
        <$> x .: "color"
        <*> x .: "default"
        <*> x .: "url"
        <*> x .: "name"
        <*> x .: "id"
        <*> x .: "node_id"

    parseJSON _ = fail "Label"


instance ToJSON Label where
    toJSON Label{..} = object
        [ "color"   .= labelColor
        , "default" .= labelDefault
        , "url"     .= labelUrl
        , "name"    .= labelName
        , "id"      .= labelId
        , "node_id" .= labelNodeId
        ]


instance Arbitrary Label where
    arbitrary = Label
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
