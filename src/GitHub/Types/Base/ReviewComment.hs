{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.ReviewComment where

import           Control.Applicative                  ((<$>), (<*>))
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..), object)
import           Data.Aeson.Types                     (Value (..), (.:), (.=))
import           Data.Text                            (Text)
import           Test.QuickCheck.Arbitrary            (Arbitrary (..))

import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.ReviewCommentLinks
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- ReviewComment

data ReviewComment = ReviewComment
    { reviewCommentAuthorAssociation   :: Text
    , reviewCommentBody                :: Text
    , reviewCommentCommitId            :: Text
    , reviewCommentCreatedAt           :: DateTime
    , reviewCommentDiffHunk            :: Text
    , reviewCommentHtmlUrl             :: Text
    , reviewCommentId                  :: Int
    , reviewCommentLine                :: Int
    , reviewCommentLinks               :: ReviewCommentLinks
    , reviewCommentNodeId              :: Text
    , reviewCommentOriginalCommitId    :: Text
    , reviewCommentOriginalLine        :: Int
    , reviewCommentOriginalPosition    :: Maybe Int
    , reviewCommentOriginalStartLine   :: Maybe Int
    , reviewCommentPath                :: Maybe Text
    , reviewCommentPosition            :: Maybe Int
    , reviewCommentPullRequestReviewId :: Int
    , reviewCommentPullRequestUrl      :: Text
    , reviewCommentSide                :: Text
    , reviewCommentStartLine           :: Maybe Int
    , reviewCommentStartSide           :: Maybe Text
    , reviewCommentUpdatedAt           :: DateTime
    , reviewCommentUrl                 :: Text
    , reviewCommentUser                :: User
    } deriving (Eq, Show, Read)


instance FromJSON ReviewComment where
    parseJSON (Object x) = ReviewComment
        <$> x .: "author_association"
        <*> x .: "body"
        <*> x .: "commit_id"
        <*> x .: "created_at"
        <*> x .: "diff_hunk"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "line"
        <*> x .: "_links"
        <*> x .: "node_id"
        <*> x .: "original_commit_id"
        <*> x .: "original_line"
        <*> x .: "original_position"
        <*> x .: "original_start_line"
        <*> x .: "path"
        <*> x .: "position"
        <*> x .: "pull_request_review_id"
        <*> x .: "pull_request_url"
        <*> x .: "side"
        <*> x .: "start_line"
        <*> x .: "start_side"
        <*> x .: "updated_at"
        <*> x .: "url"
        <*> x .: "user"

    parseJSON _ = fail "ReviewComment"


instance ToJSON ReviewComment where
    toJSON ReviewComment{..} = object
        [ "author_association"     .= reviewCommentAuthorAssociation
        , "body"                   .= reviewCommentBody
        , "commit_id"              .= reviewCommentCommitId
        , "created_at"             .= reviewCommentCreatedAt
        , "diff_hunk"              .= reviewCommentDiffHunk
        , "html_url"               .= reviewCommentHtmlUrl
        , "id"                     .= reviewCommentId
        , "line"                   .= reviewCommentLine
        , "_links"                 .= reviewCommentLinks
        , "node_id"                .= reviewCommentNodeId
        , "original_commit_id"     .= reviewCommentOriginalCommitId
        , "original_line"          .= reviewCommentOriginalLine
        , "original_position"      .= reviewCommentOriginalPosition
        , "original_start_line"    .= reviewCommentOriginalStartLine
        , "path"                   .= reviewCommentPath
        , "position"               .= reviewCommentPosition
        , "pull_request_review_id" .= reviewCommentPullRequestReviewId
        , "pull_request_url"       .= reviewCommentPullRequestUrl
        , "side"                   .= reviewCommentSide
        , "start_line"             .= reviewCommentStartLine
        , "start_side"             .= reviewCommentStartSide
        , "updated_at"             .= reviewCommentUpdatedAt
        , "url"                    .= reviewCommentUrl
        , "user"                   .= reviewCommentUser
        ]


instance Arbitrary ReviewComment where
    arbitrary = ReviewComment
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
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
