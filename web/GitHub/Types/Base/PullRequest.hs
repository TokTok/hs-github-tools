{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.PullRequest where

import           Control.Applicative                ((<$>), (<*>))
import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     object)
import           Data.Aeson.Types                   (Value (..), (.:), (.=))
import           Data.Text                          (Text)

import           GitHub.Types.Base.Commit
import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.Milestone
import           GitHub.Types.Base.PullRequestLinks
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- PullRequest

data PullRequest = PullRequest
    { pullRequestMerged            :: Bool
    , pullRequestAdditions         :: Int
    , pullRequestState             :: Text
    , pullRequestMergeableState    :: Text
    , pullRequestReviewCommentUrl  :: Text
    , pullRequestMergeable         :: Maybe Bool
    , pullRequestAssignees         :: [User]
    , pullRequestLocked            :: Bool
    , pullRequestBase              :: Commit
    , pullRequestBody              :: Text
    , pullRequestHead              :: Commit
    , pullRequestUrl               :: Text
    , pullRequestMilestone         :: Maybe Milestone
    , pullRequestStatusesUrl       :: Text
    , pullRequestMergedAt          :: Maybe DateTime
    , pullRequestCommitsUrl        :: Text
    , pullRequestAssignee          :: Maybe User
    , pullRequestDiffUrl           :: Text
    , pullRequestUser              :: User
    , pullRequestCommentsUrl       :: Text
    , pullRequestLinks             :: PullRequestLinks
    , pullRequestUpdatedAt         :: DateTime
    , pullRequestDeletions         :: Int
    , pullRequestCommits           :: Int
    , pullRequestPatchUrl          :: Text
    , pullRequestCreatedAt         :: DateTime
    , pullRequestReviewComments    :: Int
    , pullRequestId                :: Int
    , pullRequestIssueUrl          :: Text
    , pullRequestComments          :: Int
    , pullRequestMergedBy          :: Maybe User
    , pullRequestTitle             :: Text
    , pullRequestClosedAt          :: Maybe DateTime
    , pullRequestChangedFiles      :: Int
    , pullRequestNumber            :: Int
    , pullRequestMergeCommitSha    :: Maybe Text
    , pullRequestReviewCommentsUrl :: Text
    , pullRequestHtmlUrl           :: Text
    } deriving (Eq, Show, Read)


instance FromJSON PullRequest where
    parseJSON (Object x) = PullRequest
        <$> x .: "merged"
        <*> x .: "additions"
        <*> x .: "state"
        <*> x .: "mergeable_state"
        <*> x .: "review_comment_url"
        <*> x .: "mergeable"
        <*> x .: "assignees"
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
        <*> x .: "deletions"
        <*> x .: "commits"
        <*> x .: "patch_url"
        <*> x .: "created_at"
        <*> x .: "review_comments"
        <*> x .: "id"
        <*> x .: "issue_url"
        <*> x .: "comments"
        <*> x .: "merged_by"
        <*> x .: "title"
        <*> x .: "closed_at"
        <*> x .: "changed_files"
        <*> x .: "number"
        <*> x .: "merge_commit_sha"
        <*> x .: "review_comments_url"
        <*> x .: "html_url"

    parseJSON _ = fail "PullRequest"


instance ToJSON PullRequest where
    toJSON PullRequest{..} = object
        [ "merged"              .= pullRequestMerged
        , "additions"           .= pullRequestAdditions
        , "state"               .= pullRequestState
        , "mergeable_state"     .= pullRequestMergeableState
        , "review_comment_url"  .= pullRequestReviewCommentUrl
        , "mergeable"           .= pullRequestMergeable
        , "assignees"           .= pullRequestAssignees
        , "locked"              .= pullRequestLocked
        , "base"                .= pullRequestBase
        , "body"                .= pullRequestBody
        , "head"                .= pullRequestHead
        , "url"                 .= pullRequestUrl
        , "milestone"           .= pullRequestMilestone
        , "statuses_url"        .= pullRequestStatusesUrl
        , "merged_at"           .= pullRequestMergedAt
        , "commits_url"         .= pullRequestCommitsUrl
        , "assignee"            .= pullRequestAssignee
        , "diff_url"            .= pullRequestDiffUrl
        , "user"                .= pullRequestUser
        , "comments_url"        .= pullRequestCommentsUrl
        , "_links"              .= pullRequestLinks
        , "updated_at"          .= pullRequestUpdatedAt
        , "deletions"           .= pullRequestDeletions
        , "commits"             .= pullRequestCommits
        , "patch_url"           .= pullRequestPatchUrl
        , "created_at"          .= pullRequestCreatedAt
        , "review_comments"     .= pullRequestReviewComments
        , "id"                  .= pullRequestId
        , "issue_url"           .= pullRequestIssueUrl
        , "comments"            .= pullRequestComments
        , "merged_by"           .= pullRequestMergedBy
        , "title"               .= pullRequestTitle
        , "closed_at"           .= pullRequestClosedAt
        , "changed_files"       .= pullRequestChangedFiles
        , "number"              .= pullRequestNumber
        , "merge_commit_sha"    .= pullRequestMergeCommitSha
        , "review_comments_url" .= pullRequestReviewCommentsUrl
        , "html_url"            .= pullRequestHtmlUrl
        ]
