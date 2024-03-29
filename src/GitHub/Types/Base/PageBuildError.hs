{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.PageBuildError where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- PageBuildError

newtype PageBuildError = PageBuildError
    { pageBuildErrorMessage :: Maybe Text
    } deriving (Eq, Show, Read)


instance FromJSON PageBuildError where
    parseJSON (Object x) = PageBuildError
        <$> x .: "message"

    parseJSON _ = fail "PageBuildError"


instance ToJSON PageBuildError where
    toJSON PageBuildError{..} = object
        [ "message" .= pageBuildErrorMessage
        ]


instance Arbitrary PageBuildError where
    arbitrary = PageBuildError
        <$> arbitrary
