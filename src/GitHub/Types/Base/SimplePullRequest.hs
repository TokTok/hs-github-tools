{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.SimplePullRequest where

import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     object)
import           Data.Aeson.Types                   (Value (..), (.:), (.=))
import           Data.Text                          (Text)
import           Test.QuickCheck.Arbitrary          (Arbitrary (..))

import           GitHub.Types.Base.Commit
import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.Label
import           GitHub.Types.Base.Milestone
import           GitHub.Types.Base.PullRequestLinks
import           GitHub.Types.Base.Team
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- SimplePullRequest

data SimplePullRequest = SimplePullRequest
    { simplePullRequestState              :: Text
    , simplePullRequestReviewCommentUrl   :: Text
    , simplePullRequestAssignees          :: [User]
    , simplePullRequestAuthorAssociation  :: Text
    , simplePullRequestDraft              :: Bool
    , simplePullRequestLocked             :: Bool
    , simplePullRequestBase               :: Commit
    , simplePullRequestBody               :: Text
    , simplePullRequestHead               :: Commit
    , simplePullRequestUrl                :: Text
    , simplePullRequestMilestone          :: Maybe Milestone
    , simplePullRequestStatusesUrl        :: Text
    , simplePullRequestMergedAt           :: Maybe DateTime
    , simplePullRequestCommitsUrl         :: Text
    , simplePullRequestAssignee           :: Maybe User
    , simplePullRequestDiffUrl            :: Text
    , simplePullRequestUser               :: User
    , simplePullRequestCommentsUrl        :: Text
    , simplePullRequestLinks              :: PullRequestLinks
    , simplePullRequestUpdatedAt          :: DateTime
    , simplePullRequestPatchUrl           :: Text
    , simplePullRequestCreatedAt          :: DateTime
    , simplePullRequestId                 :: Int
    , simplePullRequestNodeId             :: Text
    , simplePullRequestIssueUrl           :: Text
    , simplePullRequestTitle              :: Text
    , simplePullRequestClosedAt           :: Maybe DateTime
    , simplePullRequestNumber             :: Int
    , simplePullRequestMergeCommitSha     :: Maybe Text
    , simplePullRequestReviewCommentsUrl  :: Text
    , simplePullRequestHtmlUrl            :: Text
    , simplePullRequestRequestedReviewers :: [User]
    , simplePullRequestRequestedTeams     :: [Team]
    , simplePullRequestLabels             :: [Label]
    } deriving (Eq, Show, Read)


instance FromJSON SimplePullRequest where
    parseJSON (Object x) = SimplePullRequest
        <$> x .: "state"
        <*> x .: "review_comment_url"
        <*> x .: "assignees"
        <*> x .: "author_association"
        <*> x .: "draft"
        <*> x .: "locked"
        <*> x .: "base"
        <*> x .: "body"
        <*> x .: "head"
        <*> x .: "url"
        <*> x .: "milestone"
        <*> x .: "statuses_url"
        <*> x .: "merged_at"
        <*> x .: "commits_url"
        <*> x .: "assignee"
        <*> x .: "diff_url"
        <*> x .: "user"
        <*> x .: "comments_url"
        <*> x .: "_links"
        <*> x .: "updated_at"
        <*> x .: "patch_url"
        <*> x .: "created_at"
        <*> x .: "id"
        <*> x .: "node_id"
        <*> x .: "issue_url"
        <*> x .: "title"
        <*> x .: "closed_at"
        <*> x .: "number"
        <*> x .: "merge_commit_sha"
        <*> x .: "review_comments_url"
        <*> x .: "html_url"
        <*> x .: "requested_reviewers"
        <*> x .: "requested_teams"
        <*> x .: "labels"

    parseJSON _ = fail "SimplePullRequest"


instance ToJSON SimplePullRequest where
    toJSON SimplePullRequest{..} = object
        [ "state"                .= simplePullRequestState
        , "review_comment_url"   .= simplePullRequestReviewCommentUrl
        , "assignees"            .= simplePullRequestAssignees
        , "author_association"   .= simplePullRequestAuthorAssociation
        , "draft"                .= simplePullRequestDraft
        , "locked"               .= simplePullRequestLocked
        , "base"                 .= simplePullRequestBase
        , "body"                 .= simplePullRequestBody
        , "head"                 .= simplePullRequestHead
        , "url"                  .= simplePullRequestUrl
        , "milestone"            .= simplePullRequestMilestone
        , "statuses_url"         .= simplePullRequestStatusesUrl
        , "merged_at"            .= simplePullRequestMergedAt
        , "commits_url"          .= simplePullRequestCommitsUrl
        , "assignee"             .= simplePullRequestAssignee
        , "diff_url"             .= simplePullRequestDiffUrl
        , "user"                 .= simplePullRequestUser
        , "comments_url"         .= simplePullRequestCommentsUrl
        , "_links"               .= simplePullRequestLinks
        , "updated_at"           .= simplePullRequestUpdatedAt
        , "patch_url"            .= simplePullRequestPatchUrl
        , "created_at"           .= simplePullRequestCreatedAt
        , "id"                   .= simplePullRequestId
        , "node_id"              .= simplePullRequestNodeId
        , "issue_url"            .= simplePullRequestIssueUrl
        , "title"                .= simplePullRequestTitle
        , "closed_at"            .= simplePullRequestClosedAt
        , "number"               .= simplePullRequestNumber
        , "merge_commit_sha"     .= simplePullRequestMergeCommitSha
        , "review_comments_url"  .= simplePullRequestReviewCommentsUrl
        , "html_url"             .= simplePullRequestHtmlUrl
        , "requested_reviewers"  .= simplePullRequestRequestedReviewers
        , "requested_teams"      .= simplePullRequestRequestedTeams
        , "labels"               .= simplePullRequestLabels
        ]


instance Arbitrary SimplePullRequest where
    arbitrary = SimplePullRequest
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
