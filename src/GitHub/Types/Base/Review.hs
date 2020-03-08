{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.Review where

import           Control.Applicative           ((<$>), (<*>))
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                object)
import           Data.Aeson.Types              (Value (..), (.:), (.=))
import           Data.Text                     (Text)
import           Test.QuickCheck.Arbitrary     (Arbitrary (..))

import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.ReviewLinks
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- Review

data Review = Review
    { reviewId                :: Int
    , reviewUser              :: User
    , reviewAuthorAssociation :: Text
    , reviewCommitId          :: Text
    , reviewBody              :: Maybe Text
    , reviewSubmittedAt       :: DateTime
    , reviewNodeId            :: Text
    , reviewState             :: Text
    , reviewHtmlUrl           :: Text
    , reviewPullRequestUrl    :: Text
    , reviewLinks             :: ReviewLinks
    } deriving (Eq, Show, Read)


instance FromJSON Review where
    parseJSON (Object x) = Review
        <$> x .: "id"
        <*> x .: "user"
        <*> x .: "author_association"
        <*> x .: "commit_id"
        <*> x .: "body"
        <*> x .: "submitted_at"
        <*> x .: "node_id"
        <*> x .: "state"
        <*> x .: "html_url"
        <*> x .: "pull_request_url"
        <*> x .: "_links"

    parseJSON _ = fail "Review"


instance ToJSON Review where
    toJSON Review{..} = object
        [ "id"                 .= reviewId
        , "user"               .= reviewUser
        , "author_association" .= reviewAuthorAssociation
        , "commit_id"          .= reviewCommitId
        , "body"               .= reviewBody
        , "submitted_at"       .= reviewSubmittedAt
        , "node_id"            .= reviewNodeId
        , "state"              .= reviewState
        , "html_url"           .= reviewHtmlUrl
        , "pull_request_url"   .= reviewPullRequestUrl
        , "_links"             .= reviewLinks
        ]


instance Arbitrary Review where
    arbitrary = Review
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
