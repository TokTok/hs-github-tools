{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.CheckOutput where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- CheckOutput

data CheckOutput = CheckOutput
    { checkOutputAnnotationsCount :: Int
    , checkOutputAnnotationsUrl   :: Text
    , checkOutputSummary          :: Maybe Text
    , checkOutputText             :: Maybe Text
    , checkOutputTitle            :: Maybe Text
    } deriving (Eq, Show, Read)


instance FromJSON CheckOutput where
    parseJSON (Object x) = CheckOutput
        <$> x .: "annotations_count"
        <*> x .: "annotations_url"
        <*> x .: "summary"
        <*> x .: "text"
        <*> x .: "title"

    parseJSON _ = fail "CheckOutput"


instance ToJSON CheckOutput where
    toJSON CheckOutput{..} = object
        [ "annotations_count" .= checkOutputAnnotationsCount
        , "annotations_url"   .= checkOutputAnnotationsUrl
        , "summary"           .= checkOutputSummary
        , "text"              .= checkOutputText
        , "title"             .= checkOutputTitle
        ]


instance Arbitrary CheckOutput where
    arbitrary = CheckOutput
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
