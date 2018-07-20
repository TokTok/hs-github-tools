{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.Link where

import           Control.Applicative       ((<$>))
import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Link

newtype Link = Link
    { linkHref :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Link where
    parseJSON (Object x) = Link
        <$> x .: "href"

    parseJSON _ = fail "Link"


instance ToJSON Link where
    toJSON Link{..} = object
        [ "href" .= linkHref
        ]


instance Arbitrary Link where
    arbitrary = Link
        <$> arbitrary
