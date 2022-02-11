{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.CheckPullRequest where

import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     object)
import           Data.Aeson.Types                   (Value (..), (.:), (.=))
import           Data.Text                          (Text)
import           Test.QuickCheck.Arbitrary          (Arbitrary (..))

import           GitHub.Types.Base.Commit

------------------------------------------------------------------------------
-- CheckPullRequest

data CheckPullRequest = CheckPullRequest
    { checkPullRequestBase               :: Commit
    , checkPullRequestHead               :: Commit
    , checkPullRequestId                 :: Int
    , checkPullRequestNumber             :: Int
    , checkPullRequestUrl                :: Text
    } deriving (Eq, Show, Read)


instance FromJSON CheckPullRequest where
    parseJSON (Object x) = CheckPullRequest
        <$> x .: "base"
        <*> x .: "head"
        <*> x .: "id"
        <*> x .: "number"
        <*> x .: "url"

    parseJSON _ = fail "CheckPullRequest"


instance ToJSON CheckPullRequest where
    toJSON CheckPullRequest{..} = object
        [ "base"   .= checkPullRequestBase
        , "head"   .= checkPullRequestHead
        , "id"     .= checkPullRequestId
        , "number" .= checkPullRequestNumber
        , "url"    .= checkPullRequestUrl
        ]


instance Arbitrary CheckPullRequest where
    arbitrary = CheckPullRequest
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
