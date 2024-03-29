{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Change where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Change

newtype Change = Change
    { changesFrom :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Change where
    parseJSON (Object x) = Change
        <$> x .: "from"

    parseJSON _ = fail "Change"


instance ToJSON Change where
    toJSON Change{..} = object
        [ "from" .= changesFrom
        ]


instance Arbitrary Change where
    arbitrary = Change
        <$> arbitrary
