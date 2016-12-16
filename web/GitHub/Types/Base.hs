{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GitHub.Types.Base where


import           Control.Applicative ((<$>), (<*>), (<|>))

import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.:?), (.=))
import           Data.Text           (Text)



------------------------------------------------------------------------------
-- CommitComment

data CommitComment = CommitComment
    { commitCommentUrl       :: Text
    , commitCommentHtmlUrl   :: Text
    , commitCommentId        :: Int
    , commitCommentUser      :: User
    , commitCommentPosition  :: Maybe Int
    , commitCommentLine      :: Maybe Int
    , commitCommentPath      :: Maybe Text
    , commitCommentCommitId  :: Text
    , commitCommentCreatedAt :: DateTime
    , commitCommentUpdatedAt :: DateTime
    , commitCommentBody      :: Text
    } deriving (Eq, Show, Read)


instance FromJSON CommitComment where
    parseJSON (Object x) = CommitComment
        <$> x .: "url"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "user"
        <*> x .: "position"
        <*> x .: "line"
        <*> x .: "path"
        <*> x .: "commit_id"
        <*> x .: "created_at"
        <*> x .: "updated_at"
        <*> x .: "body"

    parseJSON _ = fail "CommitComment"


instance ToJSON CommitComment where
    toJSON CommitComment{..} = object
        [ "url"        .= commitCommentUrl
        , "html_url"   .= commitCommentHtmlUrl
        , "id"         .= commitCommentId
        , "user"       .= commitCommentUser
        , "position"   .= commitCommentPosition
        , "line"       .= commitCommentLine
        , "path"       .= commitCommentPath
        , "commit_id"  .= commitCommentCommitId
        , "created_at" .= commitCommentCreatedAt
        , "updated_at" .= commitCommentUpdatedAt
        , "body"       .= commitCommentBody
        ]



------------------------------------------------------------------------------
-- IssueComment

data IssueComment = IssueComment
    { issueCommentBody      :: Text
    , issueCommentUrl       :: Text
    , issueCommentUser      :: User
    , issueCommentUpdatedAt :: DateTime
    , issueCommentCreatedAt :: DateTime
    , issueCommentId        :: Int
    , issueCommentIssueUrl  :: Text
    , issueCommentHtmlUrl   :: Text
    } deriving (Eq, Show, Read)


instance FromJSON IssueComment where
    parseJSON (Object x) = IssueComment
        <$> x .: "body"
        <*> x .: "url"
        <*> x .: "user"
        <*> x .: "updated_at"
        <*> x .: "created_at"
        <*> x .: "id"
        <*> x .: "issue_url"
        <*> x .: "html_url"

    parseJSON _ = fail "IssueComment"


instance ToJSON IssueComment where
    toJSON IssueComment{..} = object
        [ "body"       .= issueCommentBody
        , "url"        .= issueCommentUrl
        , "user"       .= issueCommentUser
        , "updated_at" .= issueCommentUpdatedAt
        , "created_at" .= issueCommentCreatedAt
        , "id"         .= issueCommentId
        , "issue_url"  .= issueCommentIssueUrl
        , "html_url"   .= issueCommentHtmlUrl
        ]



------------------------------------------------------------------------------
-- Milestone

data Milestone = Milestone
    { milestoneCreator      :: User
    , milestoneClosedIssues :: Int
    , milestoneState        :: Text
    , milestoneDueOn        :: Text
    , milestoneUrl          :: Text
    , milestoneUpdatedAt    :: DateTime
    , milestoneCreatedAt    :: DateTime
    , milestoneId           :: Int
    , milestoneTitle        :: Text
    , milestoneClosedAt     :: Maybe DateTime
    , milestoneNumber       :: Int
    , milestoneDescription  :: Text
    , milestoneLabelsUrl    :: Text
    , milestoneHtmlUrl      :: Text
    , milestoneOpenIssues   :: Int
    } deriving (Eq, Show, Read)


instance FromJSON Milestone where
    parseJSON (Object x) = Milestone
        <$> x .: "creator"
        <*> x .: "closed_issues"
        <*> x .: "state"
        <*> x .: "due_on"
        <*> x .: "url"
        <*> x .: "updated_at"
        <*> x .: "created_at"
        <*> x .: "id"
        <*> x .: "title"
        <*> x .: "closed_at"
        <*> x .: "number"
        <*> x .: "description"
        <*> x .: "labels_url"
        <*> x .: "html_url"
        <*> x .: "open_issues"

    parseJSON _ = fail "Milestone"


instance ToJSON Milestone where
    toJSON Milestone{..} = object
        [ "creator"       .= milestoneCreator
        , "closed_issues" .= milestoneClosedIssues
        , "state"         .= milestoneState
        , "due_on"        .= milestoneDueOn
        , "url"           .= milestoneUrl
        , "updated_at"    .= milestoneUpdatedAt
        , "created_at"    .= milestoneCreatedAt
        , "id"            .= milestoneId
        , "title"         .= milestoneTitle
        , "closed_at"     .= milestoneClosedAt
        , "number"        .= milestoneNumber
        , "description"   .= milestoneDescription
        , "labels_url"    .= milestoneLabelsUrl
        , "html_url"      .= milestoneHtmlUrl
        , "open_issues"   .= milestoneOpenIssues
        ]



------------------------------------------------------------------------------
-- Label

data Label = Label
    { labelColor   :: Text
    , labelDefault :: Bool
    , labelUrl     :: Text
    , labelName    :: Text
    , labelId      :: Int
    } deriving (Eq, Show, Read)


instance FromJSON Label where
    parseJSON (Object x) = Label
        <$> x .: "color"
        <*> x .: "default"
        <*> x .: "url"
        <*> x .: "name"
        <*> x .: "id"

    parseJSON _ = fail "Label"


instance ToJSON Label where
    toJSON Label{..} = object
        [ "color"   .= labelColor
        , "default" .= labelDefault
        , "url"     .= labelUrl
        , "name"    .= labelName
        , "id"      .= labelId
        ]



------------------------------------------------------------------------------
-- PullRequestRef

data PullRequestRef = PullRequestRef
    { pullRequestRefUrl      :: Text
    , pullRequestRefDiffUrl  :: Text
    , pullRequestRefPatchUrl :: Text
    , pullRequestRefHtmlUrl  :: Text
    } deriving (Eq, Show, Read)


instance FromJSON PullRequestRef where
    parseJSON (Object x) = PullRequestRef
        <$> x .: "url"
        <*> x .: "diff_url"
        <*> x .: "patch_url"
        <*> x .: "html_url"

    parseJSON _ = fail "PullRequestRef"


instance ToJSON PullRequestRef where
    toJSON PullRequestRef{..} = object
        [ "url"       .= pullRequestRefUrl
        , "diff_url"  .= pullRequestRefDiffUrl
        , "patch_url" .= pullRequestRefPatchUrl
        , "html_url"  .= pullRequestRefHtmlUrl
        ]



------------------------------------------------------------------------------
-- Change

data Change = Change
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



------------------------------------------------------------------------------
-- Changes

data Changes = Changes
    { changesTitle :: Maybe Change
    , changesBody  :: Maybe Change
    } deriving (Eq, Show, Read)


instance FromJSON Changes where
    parseJSON (Object x) = Changes
        <$> x .:? "title"
        <*> x .:? "body"

    parseJSON _ = fail "Changes"


instance ToJSON Changes where
    toJSON Changes{..} = object
        [ "title" .= changesTitle
        , "body"  .= changesBody
        ]



------------------------------------------------------------------------------
-- DateTime

data DateTime
    = DateTimeStamp Int
    | DateTimeText Text
    deriving (Eq, Show, Read)


instance FromJSON DateTime where
    parseJSON x =
          DateTimeStamp <$> parseJSON x
      <|> DateTimeText  <$> parseJSON x


instance ToJSON DateTime where
    toJSON (DateTimeStamp x) = toJSON x
    toJSON (DateTimeText  x) = toJSON x



------------------------------------------------------------------------------
-- RepoOwner

data RepoOwner
    = RepoOwnerUser User
    | RepoOwnerUserRef UserRef
    deriving (Eq, Show, Read)


instance FromJSON RepoOwner where
    parseJSON x =
          RepoOwnerUser    <$> parseJSON x
      <|> RepoOwnerUserRef <$> parseJSON x


instance ToJSON RepoOwner where
    toJSON (RepoOwnerUser    x) = toJSON x
    toJSON (RepoOwnerUserRef x) = toJSON x



------------------------------------------------------------------------------
-- Review

data Review = Review
    { reviewId             :: Int
    , reviewUser           :: User
    , reviewBody           :: Text
    , reviewSubmittedAt    :: DateTime
    , reviewState          :: Text
    , reviewHtmlUrl        :: Text
    , reviewPullRequestUrl :: Text
    , reviewLinks          :: ReviewLinks
    } deriving (Eq, Show, Read)


instance FromJSON Review where
    parseJSON (Object x) = Review
        <$> x .: "id"
        <*> x .: "user"
        <*> x .: "body"
        <*> x .: "submitted_at"
        <*> x .: "state"
        <*> x .: "html_url"
        <*> x .: "pull_request_url"
        <*> x .: "_links"

    parseJSON _ = fail "Review"


instance ToJSON Review where
    toJSON Review{..} = object
        [ "id"               .= reviewId
        , "user"             .= reviewUser
        , "body"             .= reviewBody
        , "submitted_at"     .= reviewSubmittedAt
        , "state"            .= reviewState
        , "html_url"         .= reviewHtmlUrl
        , "pull_request_url" .= reviewPullRequestUrl
        , "_links"           .= reviewLinks
        ]



------------------------------------------------------------------------------
-- Link

data Link = Link
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



------------------------------------------------------------------------------
-- ReviewLinks

data ReviewLinks = ReviewLinks
    { reviewLinksHtml        :: Link
    , reviewLinksPullRequest :: Link
    } deriving (Eq, Show, Read)


instance FromJSON ReviewLinks where
    parseJSON (Object x) = ReviewLinks
        <$> x .: "html"
        <*> x .: "pull_request"

    parseJSON _ = fail "ReviewLinks"


instance ToJSON ReviewLinks where
    toJSON ReviewLinks{..} = object
        [ "html"         .= reviewLinksHtml
        , "pull_request" .= reviewLinksPullRequest
        ]



------------------------------------------------------------------------------
-- PullRequestLinks

data PullRequestLinks = PullRequestLinks
    { pullRequestLinksSelf           :: Link
    , pullRequestLinksCommits        :: Link
    , pullRequestLinksStatuses       :: Link
    , pullRequestLinksReviewComments :: Link
    , pullRequestLinksHtml           :: Link
    , pullRequestLinksComments       :: Link
    , pullRequestLinksReviewComment  :: Link
    , pullRequestLinksIssue          :: Link
    } deriving (Eq, Show, Read)


instance FromJSON PullRequestLinks where
    parseJSON (Object x) = PullRequestLinks
        <$> x .: "self"
        <*> x .: "commits"
        <*> x .: "statuses"
        <*> x .: "review_comments"
        <*> x .: "html"
        <*> x .: "comments"
        <*> x .: "review_comment"
        <*> x .: "issue"

    parseJSON _ = fail "PullRequestLinks"


instance ToJSON PullRequestLinks where
    toJSON PullRequestLinks{..} = object
        [ "self"            .= pullRequestLinksSelf
        , "commits"         .= pullRequestLinksCommits
        , "statuses"        .= pullRequestLinksStatuses
        , "review_comments" .= pullRequestLinksReviewComments
        , "html"            .= pullRequestLinksHtml
        , "comments"        .= pullRequestLinksComments
        , "review_comment"  .= pullRequestLinksReviewComment
        , "issue"           .= pullRequestLinksIssue
        ]



------------------------------------------------------------------------------
-- Issue

data Issue = Issue
    { issueState         :: Text
    , issueAssignees     :: [User]
    , issueLocked        :: Bool
    , issueBody          :: Text
    , issueUrl           :: Text
    , issuePullRequest   :: Maybe PullRequestRef
    , issueMilestone     :: Maybe Milestone
    , issueAssignee      :: Maybe User
    , issueUser          :: User
    , issueCommentsUrl   :: Text
    , issueUpdatedAt     :: DateTime
    , issueCreatedAt     :: DateTime
    , issueId            :: Int
    , issueLabels        :: [Label]
    , issueComments      :: Int
    , issueTitle         :: Text
    , issueClosedAt      :: Maybe DateTime
    , issueNumber        :: Int
    , issueEventsUrl     :: Text
    , issueRepositoryUrl :: Text
    , issueLabelsUrl     :: Text
    , issueHtmlUrl       :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Issue where
    parseJSON (Object x) = Issue
        <$> x .: "state"
        <*> x .: "assignees"
        <*> x .: "locked"
        <*> x .: "body"
        <*> x .: "url"
        <*> x .:? "pull_request"
        <*> x .: "milestone"
        <*> x .: "assignee"
        <*> x .: "user"
        <*> x .: "comments_url"
        <*> x .: "updated_at"
        <*> x .: "created_at"
        <*> x .: "id"
        <*> x .: "labels"
        <*> x .: "comments"
        <*> x .: "title"
        <*> x .: "closed_at"
        <*> x .: "number"
        <*> x .: "events_url"
        <*> x .: "repository_url"
        <*> x .: "labels_url"
        <*> x .: "html_url"

    parseJSON _ = fail "Issue"


instance ToJSON Issue where
    toJSON Issue{..} = object
        [ "state"          .= issueState
        , "assignees"      .= issueAssignees
        , "locked"         .= issueLocked
        , "body"           .= issueBody
        , "url"            .= issueUrl
        , "pull_request"   .= issuePullRequest
        , "milestone"      .= issueMilestone
        , "assignee"       .= issueAssignee
        , "user"           .= issueUser
        , "comments_url"   .= issueCommentsUrl
        , "updated_at"     .= issueUpdatedAt
        , "created_at"     .= issueCreatedAt
        , "id"             .= issueId
        , "labels"         .= issueLabels
        , "comments"       .= issueComments
        , "title"          .= issueTitle
        , "closed_at"      .= issueClosedAt
        , "number"         .= issueNumber
        , "events_url"     .= issueEventsUrl
        , "repository_url" .= issueRepositoryUrl
        , "labels_url"     .= issueLabelsUrl
        , "html_url"       .= issueHtmlUrl
        ]



------------------------------------------------------------------------------
-- SimplePullRequest

data SimplePullRequest = SimplePullRequest
    { simplePullRequestState             :: Text
    , simplePullRequestReviewCommentUrl  :: Text
    , simplePullRequestAssignees         :: [User]
    , simplePullRequestLocked            :: Bool
    , simplePullRequestBase              :: Commit
    , simplePullRequestBody              :: Text
    , simplePullRequestHead              :: Commit
    , simplePullRequestUrl               :: Text
    , simplePullRequestMilestone         :: Maybe Milestone
    , simplePullRequestStatusesUrl       :: Text
    , simplePullRequestMergedAt          :: Maybe DateTime
    , simplePullRequestCommitsUrl        :: Text
    , simplePullRequestAssignee          :: Maybe User
    , simplePullRequestDiffUrl           :: Text
    , simplePullRequestUser              :: User
    , simplePullRequestCommentsUrl       :: Text
    , simplePullRequestLinks             :: PullRequestLinks
    , simplePullRequestUpdatedAt         :: DateTime
    , simplePullRequestPatchUrl          :: Text
    , simplePullRequestCreatedAt         :: DateTime
    , simplePullRequestId                :: Int
    , simplePullRequestIssueUrl          :: Text
    , simplePullRequestTitle             :: Text
    , simplePullRequestClosedAt          :: Maybe DateTime
    , simplePullRequestNumber            :: Int
    , simplePullRequestMergeCommitSha    :: Maybe Text
    , simplePullRequestReviewCommentsUrl :: Text
    , simplePullRequestHtmlUrl           :: Text
    } deriving (Eq, Show, Read)


instance FromJSON SimplePullRequest where
    parseJSON (Object x) = SimplePullRequest
        <$> x .: "state"
        <*> x .: "review_comment_url"
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
        <*> x .: "patch_url"
        <*> x .: "created_at"
        <*> x .: "id"
        <*> x .: "issue_url"
        <*> x .: "title"
        <*> x .: "closed_at"
        <*> x .: "number"
        <*> x .: "merge_commit_sha"
        <*> x .: "review_comments_url"
        <*> x .: "html_url"

    parseJSON _ = fail "SimplePullRequest"


instance ToJSON SimplePullRequest where
    toJSON SimplePullRequest{..} = object
        [ "state"               .= simplePullRequestState
        , "review_comment_url"  .= simplePullRequestReviewCommentUrl
        , "assignees"           .= simplePullRequestAssignees
        , "locked"              .= simplePullRequestLocked
        , "base"                .= simplePullRequestBase
        , "body"                .= simplePullRequestBody
        , "head"                .= simplePullRequestHead
        , "url"                 .= simplePullRequestUrl
        , "milestone"           .= simplePullRequestMilestone
        , "statuses_url"        .= simplePullRequestStatusesUrl
        , "merged_at"           .= simplePullRequestMergedAt
        , "commits_url"         .= simplePullRequestCommitsUrl
        , "assignee"            .= simplePullRequestAssignee
        , "diff_url"            .= simplePullRequestDiffUrl
        , "user"                .= simplePullRequestUser
        , "comments_url"        .= simplePullRequestCommentsUrl
        , "_links"              .= simplePullRequestLinks
        , "updated_at"          .= simplePullRequestUpdatedAt
        , "patch_url"           .= simplePullRequestPatchUrl
        , "created_at"          .= simplePullRequestCreatedAt
        , "id"                  .= simplePullRequestId
        , "issue_url"           .= simplePullRequestIssueUrl
        , "title"               .= simplePullRequestTitle
        , "closed_at"           .= simplePullRequestClosedAt
        , "number"              .= simplePullRequestNumber
        , "merge_commit_sha"    .= simplePullRequestMergeCommitSha
        , "review_comments_url" .= simplePullRequestReviewCommentsUrl
        , "html_url"            .= simplePullRequestHtmlUrl
        ]



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



------------------------------------------------------------------------------
-- StatusCommit

data StatusCommit = StatusCommit
    { statusCommitSha         :: Text
    , statusCommitCommit      :: CommitDetails
    , statusCommitUrl         :: Text
    , statusCommitCommentsUrl :: Text
    , statusCommitHtmlUrl     :: Text
    , statusCommitAuthor      :: User
    , statusCommitCommitter   :: User
    , statusCommitParents     :: [CommitRefHtml]
    } deriving (Eq, Show, Read)


instance FromJSON StatusCommit where
    parseJSON (Object x) = StatusCommit
        <$> x .: "sha"
        <*> x .: "commit"
        <*> x .: "url"
        <*> x .: "comments_url"
        <*> x .: "html_url"
        <*> x .: "author"
        <*> x .: "committer"
        <*> x .: "parents"

    parseJSON _ = fail "StatusCommit"


instance ToJSON StatusCommit where
    toJSON StatusCommit{..} = object
        [ "sha"          .= statusCommitSha
        , "commit"       .= statusCommitCommit
        , "url"          .= statusCommitUrl
        , "comments_url" .= statusCommitCommentsUrl
        , "html_url"     .= statusCommitHtmlUrl
        , "author"       .= statusCommitAuthor
        , "committer"    .= statusCommitCommitter
        , "parents"      .= statusCommitParents
        ]



------------------------------------------------------------------------------
-- PushCommit

data PushCommit = PushCommit
    { pushCommitId        :: Text
    , pushCommitTreeId    :: Text
    , pushCommitDistinct  :: Bool
    , pushCommitMessage   :: Text
    , pushCommitTimestamp :: Text
    , pushCommitUrl       :: Text
    , pushCommitAuthor    :: Author
    , pushCommitCommitter :: Author
    , pushCommitAdded     :: [Text]
    , pushCommitRemoved   :: [Text]
    , pushCommitModified  :: [Text]
    } deriving (Eq, Show, Read)


instance FromJSON PushCommit where
    parseJSON (Object x) = PushCommit
        <$> x .: "id"
        <*> x .: "tree_id"
        <*> x .: "distinct"
        <*> x .: "message"
        <*> x .: "timestamp"
        <*> x .: "url"
        <*> x .: "author"
        <*> x .: "committer"
        <*> x .: "added"
        <*> x .: "removed"
        <*> x .: "modified"

    parseJSON _ = fail "PushCommit"


instance ToJSON PushCommit where
    toJSON PushCommit{..} = object
        [ "id"           .= pushCommitId
        , "tree_id"      .= pushCommitTreeId
        , "distinct"     .= pushCommitDistinct
        , "message"      .= pushCommitMessage
        , "timestamp"    .= pushCommitTimestamp
        , "url"          .= pushCommitUrl
        , "author"       .= pushCommitAuthor
        , "committer"    .= pushCommitCommitter
        , "added"        .= pushCommitAdded
        , "removed"      .= pushCommitRemoved
        , "modified"     .= pushCommitModified
        ]



------------------------------------------------------------------------------
-- Organization

data Organization = Organization
    { organizationLogin            :: Text
    , organizationId               :: Int
    , organizationUrl              :: Text
    , organizationReposUrl         :: Text
    , organizationEventsUrl        :: Text
    , organizationHooksUrl         :: Text
    , organizationIssuesUrl        :: Text
    , organizationMembersUrl       :: Text
    , organizationPublicMembersUrl :: Text
    , organizationAvatarUrl        :: Text
    , organizationDescription      :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Organization where
    parseJSON (Object x) = Organization
        <$> x .: "login"
        <*> x .: "id"
        <*> x .: "url"
        <*> x .: "repos_url"
        <*> x .: "events_url"
        <*> x .: "hooks_url"
        <*> x .: "issues_url"
        <*> x .: "members_url"
        <*> x .: "public_members_url"
        <*> x .: "avatar_url"
        <*> x .: "description"

    parseJSON _ = fail "Organization"


instance ToJSON Organization where
    toJSON Organization{..} = object
        [ "login"              .= organizationLogin
        , "id"                 .= organizationId
        , "url"                .= organizationUrl
        , "repos_url"          .= organizationReposUrl
        , "events_url"         .= organizationEventsUrl
        , "hooks_url"          .= organizationHooksUrl
        , "issues_url"         .= organizationIssuesUrl
        , "members_url"        .= organizationMembersUrl
        , "public_members_url" .= organizationPublicMembersUrl
        , "avatar_url"         .= organizationAvatarUrl
        , "description"        .= organizationDescription
        ]



------------------------------------------------------------------------------
-- User

data User = User
    { userLogin             :: Text
    , userId                :: Int
    , userAvatarUrl         :: Text
    , userGravatarId        :: Text
    , userUrl               :: Text
    , userHtmlUrl           :: Text
    , userFollowersUrl      :: Text
    , userFollowingUrl      :: Text
    , userGistsUrl          :: Text
    , userStarredUrl        :: Text
    , userSubscriptionsUrl  :: Text
    , userOrganizationsUrl  :: Text
    , userReposUrl          :: Text
    , userEventsUrl         :: Text
    , userReceivedEventsUrl :: Text
    , userType              :: Text
    , userSiteAdmin         :: Bool
    } deriving (Eq, Show, Read)


instance FromJSON User where
    parseJSON (Object x) = User
        <$> x .: "login"
        <*> x .: "id"
        <*> x .: "avatar_url"
        <*> x .: "gravatar_id"
        <*> x .: "url"
        <*> x .: "html_url"
        <*> x .: "followers_url"
        <*> x .: "following_url"
        <*> x .: "gists_url"
        <*> x .: "starred_url"
        <*> x .: "subscriptions_url"
        <*> x .: "organizations_url"
        <*> x .: "repos_url"
        <*> x .: "events_url"
        <*> x .: "received_events_url"
        <*> x .: "type"
        <*> x .: "site_admin"

    parseJSON _ = fail "User"


instance ToJSON User where
    toJSON User{..} = object
        [ "login"               .= userLogin
        , "id"                  .= userId
        , "avatar_url"          .= userAvatarUrl
        , "gravatar_id"         .= userGravatarId
        , "url"                 .= userUrl
        , "html_url"            .= userHtmlUrl
        , "followers_url"       .= userFollowersUrl
        , "following_url"       .= userFollowingUrl
        , "gists_url"           .= userGistsUrl
        , "starred_url"         .= userStarredUrl
        , "subscriptions_url"   .= userSubscriptionsUrl
        , "organizations_url"   .= userOrganizationsUrl
        , "repos_url"           .= userReposUrl
        , "events_url"          .= userEventsUrl
        , "received_events_url" .= userReceivedEventsUrl
        , "type"                .= userType
        , "site_admin"          .= userSiteAdmin
        ]



------------------------------------------------------------------------------
-- Commit

data Commit = Commit
    { commitSha   :: Text
    , commitUser  :: User
    , commitRepo  :: Repository
    , commitLabel :: Text
    , commitRef   :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Commit where
    parseJSON (Object x) = Commit
        <$> x .: "sha"
        <*> x .: "user"
        <*> x .: "repo"
        <*> x .: "label"
        <*> x .: "ref"

    parseJSON _ = fail "Commit"


instance ToJSON Commit where
    toJSON Commit{..} = object
        [ "sha"   .= commitSha
        , "user"  .= commitUser
        , "repo"  .= commitRepo
        , "label" .= commitLabel
        , "ref"   .= commitRef
        ]



------------------------------------------------------------------------------
-- CommitDetails

data CommitDetails = CommitDetails
    { commitDetailsAuthor       :: UserStamp
    , commitDetailsCommitter    :: UserStamp
    , commitDetailsMessage      :: Text
    , commitDetailsTree         :: CommitRef
    , commitDetailsUrl          :: Text
    , commitDetailsCommentCount :: Int
    } deriving (Eq, Show, Read)


instance FromJSON CommitDetails where
    parseJSON (Object x) = CommitDetails
        <$> x .: "author"
        <*> x .: "committer"
        <*> x .: "message"
        <*> x .: "tree"
        <*> x .: "url"
        <*> x .: "comment_count"

    parseJSON _ = fail "CommitDetails"


instance ToJSON CommitDetails where
    toJSON CommitDetails{..} = object
        [ "author"        .= commitDetailsAuthor
        , "committer"     .= commitDetailsCommitter
        , "message"       .= commitDetailsMessage
        , "tree"          .= commitDetailsTree
        , "url"           .= commitDetailsUrl
        , "comment_count" .= commitDetailsCommentCount
        ]



------------------------------------------------------------------------------
-- CommitRefHtml

data CommitRefHtml = CommitRefHtml
    { commitRefHtmlSha     :: Text
    , commitRefHtmlUrl     :: Text
    , commitRefHtmlHtmlUrl :: Text
    } deriving (Eq, Show, Read)


instance FromJSON CommitRefHtml where
    parseJSON (Object x) = CommitRefHtml
        <$> x .: "sha"
        <*> x .: "url"
        <*> x .: "html_url"

    parseJSON _ = fail "CommitRefHtml"


instance ToJSON CommitRefHtml where
    toJSON CommitRefHtml{..} = object
        [ "sha"      .= commitRefHtmlSha
        , "url"      .= commitRefHtmlUrl
        , "html_url" .= commitRefHtmlHtmlUrl
        ]



------------------------------------------------------------------------------
-- CommitRef

data CommitRef = CommitRef
    { commitRefSha :: Text
    , commitRefUrl :: Text
    } deriving (Eq, Show, Read)


instance FromJSON CommitRef where
    parseJSON (Object x) = CommitRef
        <$> x .: "sha"
        <*> x .: "url"

    parseJSON _ = fail "CommitRef"


instance ToJSON CommitRef where
    toJSON CommitRef{..} = object
        [ "sha" .= commitRefSha
        , "url" .= commitRefUrl
        ]



------------------------------------------------------------------------------
-- Branch

data Branch = Branch
    { branchName   :: Text
    , branchCommit :: CommitRef
    } deriving (Eq, Show, Read)


instance FromJSON Branch where
    parseJSON (Object x) = Branch
        <$> x .: "name"
        <*> x .: "commit"

    parseJSON _ = fail "Branch"


instance ToJSON Branch where
    toJSON Branch{..} = object
        [ "name"   .= branchName
        , "commit" .= branchCommit
        ]



------------------------------------------------------------------------------
-- Pusher

data Pusher = Pusher
    { pusherName  :: Text
    , pusherEmail :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Pusher where
    parseJSON (Object x) = Pusher
        <$> x .: "name"
        <*> x .: "email"

    parseJSON _ = fail "Pusher"


instance ToJSON Pusher where
    toJSON Pusher{..} = object
        [ "name"     .= pusherName
        , "email"    .= pusherEmail
        ]



------------------------------------------------------------------------------
-- UserRef

data UserRef = UserRef
    { userRefName  :: Text
    , userRefEmail :: Text
    } deriving (Eq, Show, Read)


instance FromJSON UserRef where
    parseJSON (Object x) = UserRef
        <$> x .: "name"
        <*> x .: "email"

    parseJSON _ = fail "UserRef"


instance ToJSON UserRef where
    toJSON UserRef{..} = object
        [ "name"  .= userRefName
        , "email" .= userRefEmail
        ]



------------------------------------------------------------------------------
-- UserStamp

data UserStamp = UserStamp
    { userStampName  :: Text
    , userStampEmail :: Text
    , userStampDate  :: Text
    } deriving (Eq, Show, Read)


instance FromJSON UserStamp where
    parseJSON (Object x) = UserStamp
        <$> x .: "name"
        <*> x .: "email"
        <*> x .: "date"

    parseJSON _ = fail "UserStamp"


instance ToJSON UserStamp where
    toJSON UserStamp{..} = object
        [ "name"  .= userStampName
        , "email" .= userStampEmail
        , "date"  .= userStampDate
        ]



------------------------------------------------------------------------------
-- Author

data Author = Author
    { authorName     :: Text
    , authorEmail    :: Text
    , authorUsername :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Author where
    parseJSON (Object x) = Author
        <$> x .: "name"
        <*> x .: "email"
        <*> x .: "username"

    parseJSON _ = fail "Author"


instance ToJSON Author where
    toJSON Author{..} = object
        [ "name"     .= authorName
        , "email"    .= authorEmail
        , "username" .= authorUsername
        ]



------------------------------------------------------------------------------
-- Repository

data Repository = Repository
    { repositoryHomepage         :: Maybe Text
    , repositoryHooksUrl         :: Text
    , repositoryBlobsUrl         :: Text
    , repositorySshUrl           :: Text
    , repositorySvnUrl           :: Text
    , repositoryCloneUrl         :: Text
    , repositoryMergesUrl        :: Text
    , repositoryNotificationsUrl :: Text
    , repositoryCollaboratorsUrl :: Text
    , repositoryLanguagesUrl     :: Text
    , repositorySize             :: Int
    , repositoryIssueEventsUrl   :: Text
    , repositoryPrivate          :: Bool
    , repositoryFork             :: Bool
    , repositoryGitCommitsUrl    :: Text
    , repositoryDownloadsUrl     :: Text
    , repositoryFullName         :: Text
    , repositoryUrl              :: Text
    , repositoryArchiveUrl       :: Text
    , repositoryGitUrl           :: Text
    , repositoryStatusesUrl      :: Text
    , repositoryIssuesUrl        :: Text
    , repositoryDeploymentsUrl   :: Text
    , repositoryCommitsUrl       :: Text
    , repositoryTreesUrl         :: Text
    , repositoryOwner            :: RepoOwner
    , repositoryMilestonesUrl    :: Text
    , repositoryHasWiki          :: Bool
    , repositoryIssueCommentUrl  :: Text
    , repositoryCommentsUrl      :: Text
    , repositoryContributorsUrl  :: Text
    , repositoryName             :: Text
    , repositoryHasIssues        :: Bool
    , repositoryUpdatedAt        :: DateTime
    , repositoryMasterBranch     :: Maybe Text
    , repositoryForksCount       :: Int
    , repositoryForksUrl         :: Text
    , repositorySubscriptionUrl  :: Text
    , repositoryHasDownloads     :: Bool
    , repositoryTeamsUrl         :: Text
    , repositoryPullsUrl         :: Text
    , repositoryLanguage         :: Maybe Text
    , repositoryCreatedAt        :: DateTime
    , repositoryHasPages         :: Bool
    , repositoryPushedAt         :: DateTime
    , repositoryId               :: Int
    , repositorySubscribersUrl   :: Text
    , repositoryTagsUrl          :: Text
    , repositoryOpenIssuesCount  :: Int
    , repositoryMirrorUrl        :: Maybe Text
    , repositoryWatchers         :: Int
    , repositoryStargazers       :: Maybe Int
    , repositoryStargazersCount  :: Int
    , repositoryStargazersUrl    :: Text
    , repositoryKeysUrl          :: Text
    , repositoryGitTagsUrl       :: Text
    , repositoryDefaultBranch    :: Text
    , repositoryEventsUrl        :: Text
    , repositoryCompareUrl       :: Text
    , repositoryGitRefsUrl       :: Text
    , repositoryOrganization     :: Maybe Text
    , repositoryForks            :: Int
    , repositoryContentsUrl      :: Text
    , repositoryBranchesUrl      :: Text
    , repositoryReleasesUrl      :: Text
    , repositoryAssigneesUrl     :: Text
    , repositoryDescription      :: Text
    , repositoryWatchersCount    :: Int
    , repositoryLabelsUrl        :: Text
    , repositoryHtmlUrl          :: Text
    , repositoryOpenIssues       :: Int
    , repositoryPublic           :: Maybe Bool
    } deriving (Eq, Show, Read)


instance FromJSON Repository where
    parseJSON (Object x) = Repository
        <$> x .: "homepage"
        <*> x .: "hooks_url"
        <*> x .: "blobs_url"
        <*> x .: "ssh_url"
        <*> x .: "svn_url"
        <*> x .: "clone_url"
        <*> x .: "merges_url"
        <*> x .: "notifications_url"
        <*> x .: "collaborators_url"
        <*> x .: "languages_url"
        <*> x .: "size"
        <*> x .: "issue_events_url"
        <*> x .: "private"
        <*> x .: "fork"
        <*> x .: "git_commits_url"
        <*> x .: "downloads_url"
        <*> x .: "full_name"
        <*> x .: "url"
        <*> x .: "archive_url"
        <*> x .: "git_url"
        <*> x .: "statuses_url"
        <*> x .: "issues_url"
        <*> x .: "deployments_url"
        <*> x .: "commits_url"
        <*> x .: "trees_url"
        <*> x .: "owner"
        <*> x .: "milestones_url"
        <*> x .: "has_wiki"
        <*> x .: "issue_comment_url"
        <*> x .: "comments_url"
        <*> x .: "contributors_url"
        <*> x .: "name"
        <*> x .: "has_issues"
        <*> x .: "updated_at"
        <*> x .:? "master_branch"
        <*> x .: "forks_count"
        <*> x .: "forks_url"
        <*> x .: "subscription_url"
        <*> x .: "has_downloads"
        <*> x .: "teams_url"
        <*> x .: "pulls_url"
        <*> x .:? "language"
        <*> x .: "created_at"
        <*> x .: "has_pages"
        <*> x .: "pushed_at"
        <*> x .: "id"
        <*> x .: "subscribers_url"
        <*> x .: "tags_url"
        <*> x .: "open_issues_count"
        <*> x .: "mirror_url"
        <*> x .: "watchers"
        <*> x .:? "stargazers"
        <*> x .: "stargazers_count"
        <*> x .: "stargazers_url"
        <*> x .: "keys_url"
        <*> x .: "git_tags_url"
        <*> x .: "default_branch"
        <*> x .: "events_url"
        <*> x .: "compare_url"
        <*> x .: "git_refs_url"
        <*> x .:? "organization"
        <*> x .: "forks"
        <*> x .: "contents_url"
        <*> x .: "branches_url"
        <*> x .: "releases_url"
        <*> x .: "assignees_url"
        <*> x .: "description"
        <*> x .: "watchers_count"
        <*> x .: "labels_url"
        <*> x .: "html_url"
        <*> x .: "open_issues"
        <*> x .:? "public"

    parseJSON _ = fail "Repository"


instance ToJSON Repository where
    toJSON Repository{..} = object
        [ "homepage"          .= repositoryHomepage
        , "hooks_url"         .= repositoryHooksUrl
        , "blobs_url"         .= repositoryBlobsUrl
        , "ssh_url"           .= repositorySshUrl
        , "svn_url"           .= repositorySvnUrl
        , "clone_url"         .= repositoryCloneUrl
        , "merges_url"        .= repositoryMergesUrl
        , "notifications_url" .= repositoryNotificationsUrl
        , "collaborators_url" .= repositoryCollaboratorsUrl
        , "languages_url"     .= repositoryLanguagesUrl
        , "size"              .= repositorySize
        , "issue_events_url"  .= repositoryIssueEventsUrl
        , "private"           .= repositoryPrivate
        , "fork"              .= repositoryFork
        , "git_commits_url"   .= repositoryGitCommitsUrl
        , "downloads_url"     .= repositoryDownloadsUrl
        , "full_name"         .= repositoryFullName
        , "url"               .= repositoryUrl
        , "archive_url"       .= repositoryArchiveUrl
        , "git_url"           .= repositoryGitUrl
        , "statuses_url"      .= repositoryStatusesUrl
        , "issues_url"        .= repositoryIssuesUrl
        , "deployments_url"   .= repositoryDeploymentsUrl
        , "commits_url"       .= repositoryCommitsUrl
        , "trees_url"         .= repositoryTreesUrl
        , "owner"             .= repositoryOwner
        , "milestones_url"    .= repositoryMilestonesUrl
        , "has_wiki"          .= repositoryHasWiki
        , "issue_comment_url" .= repositoryIssueCommentUrl
        , "comments_url"      .= repositoryCommentsUrl
        , "contributors_url"  .= repositoryContributorsUrl
        , "name"              .= repositoryName
        , "has_issues"        .= repositoryHasIssues
        , "updated_at"        .= repositoryUpdatedAt
        , "master_branch"     .= repositoryMasterBranch
        , "forks_count"       .= repositoryForksCount
        , "forks_url"         .= repositoryForksUrl
        , "subscription_url"  .= repositorySubscriptionUrl
        , "has_downloads"     .= repositoryHasDownloads
        , "teams_url"         .= repositoryTeamsUrl
        , "pulls_url"         .= repositoryPullsUrl
        , "language"          .= repositoryLanguage
        , "created_at"        .= repositoryCreatedAt
        , "has_pages"         .= repositoryHasPages
        , "pushed_at"         .= repositoryPushedAt
        , "id"                .= repositoryId
        , "subscribers_url"   .= repositorySubscribersUrl
        , "tags_url"          .= repositoryTagsUrl
        , "open_issues_count" .= repositoryOpenIssuesCount
        , "mirror_url"        .= repositoryMirrorUrl
        , "watchers"          .= repositoryWatchers
        , "stargazers"        .= repositoryStargazers
        , "stargazers_count"  .= repositoryStargazersCount
        , "stargazers_url"    .= repositoryStargazersUrl
        , "keys_url"          .= repositoryKeysUrl
        , "git_tags_url"      .= repositoryGitTagsUrl
        , "default_branch"    .= repositoryDefaultBranch
        , "events_url"        .= repositoryEventsUrl
        , "compare_url"       .= repositoryCompareUrl
        , "git_refs_url"      .= repositoryGitRefsUrl
        , "forks"             .= repositoryForks
        , "organization"      .= repositoryOrganization
        , "contents_url"      .= repositoryContentsUrl
        , "branches_url"      .= repositoryBranchesUrl
        , "releases_url"      .= repositoryReleasesUrl
        , "assignees_url"     .= repositoryAssigneesUrl
        , "description"       .= repositoryDescription
        , "watchers_count"    .= repositoryWatchersCount
        , "labels_url"        .= repositoryLabelsUrl
        , "html_url"          .= repositoryHtmlUrl
        , "open_issues"       .= repositoryOpenIssues
        , "public"            .= repositoryPublic
        ]


------------------------------------------------------------------------------
-- State

data State = Pending | Success | Failure | Error
 deriving (Eq, Show, Read)


instance FromJSON State where
    parseJSON (String "pending") = return Pending
    parseJSON (String "success") = return Success
    parseJSON (String "failure") = return Failure
    parseJSON (String "error")   = return Error
    parseJSON _                  = fail "State"

instance ToJSON State where
    toJSON Pending = String "pending"
    toJSON Success = String "success"
    toJSON Failure = String "failure"
    toJSON Error   = String "error"



------------------------------------------------------------------------------
-- Deployment

data Deployment = Deployment
    { deploymentUrl           :: Text
    , deploymentId            :: Int
    , deploymentSha           :: Text
    , deploymentRef           :: Text
    , deploymentTask          :: Text
    , deploymentPayload       :: Value
    , deploymentEnvironment   :: Text
    , deploymentDescription   :: Maybe Text
    , deploymentCreator       :: User
    , deploymentCreatedAt     :: DateTime
    , deploymentUpdatedAt     :: DateTime
    , deploymentStatusesUrl   :: Text
    , deploymentRepositoryUrl :: Text
    } deriving (Eq, Show, Read)

instance FromJSON Deployment where
    parseJSON (Object x) = Deployment
        <$> x .: "url"
        <*> x .: "id"
        <*> x .: "sha"
        <*> x .: "ref"
        <*> x .: "task"
        <*> x .: "payload"
        <*> x .: "environment"
        <*> x .: "description"
        <*> x .: "creator"
        <*> x .: "created_at"
        <*> x .: "updated_at"
        <*> x .: "statuses_url"
        <*> x .: "repository_url"

    parseJSON _ = fail "Deployment"

instance ToJSON Deployment where
    toJSON Deployment{..} = object
        [ "url"            .= deploymentUrl
        , "id"             .= deploymentId
        , "sha"            .= deploymentSha
        , "ref"            .= deploymentRef
        , "task"           .= deploymentTask
        , "payload"        .= deploymentPayload
        , "environment"    .= deploymentEnvironment
        , "description"    .= deploymentDescription
        , "creator"        .= deploymentCreator
        , "created_at"     .= deploymentCreatedAt
        , "updated_at"     .= deploymentUpdatedAt
        , "statuses_url"   .= deploymentStatusesUrl
        , "repository_url" .= deploymentRepositoryUrl
        ]



------------------------------------------------------------------------------
-- DeploymentStatus

data DeploymentStatus = DeploymentStatus
    { deploymentStatusUrl           :: Text
    , deploymentStatusId            :: Int
    , deploymentStatusState         :: Text
    , deploymentStatusCreator       :: User
    , deploymentStatusDescription   :: Text
    , deploymentStatusTargetUrl     :: Text
    , deploymentStatusCreatedAt     :: DateTime
    , deploymentStatusUpdatedAt     :: DateTime
    , deploymentStatusDeploymentUrl :: Text
    , deploymentStatusRepositoryUrl :: Text
    } deriving (Eq, Show, Read)

instance FromJSON DeploymentStatus where
    parseJSON (Object x) = DeploymentStatus
        <$> x .: "url"
        <*> x .: "id"
        <*> x .: "state"
        <*> x .: "creator"
        <*> x .: "description"
        <*> x .: "target_url"
        <*> x .: "created_at"
        <*> x .: "updated_at"
        <*> x .: "deployment_url"
        <*> x .: "repository_url"

    parseJSON _ = fail "DeploymentStatus"

instance ToJSON DeploymentStatus where
    toJSON DeploymentStatus{..} = object
        [ "url"            .= deploymentStatusUrl
        , "id"             .= deploymentStatusId
        , "state"          .= deploymentStatusState
        , "creator"        .= deploymentStatusCreator
        , "description"    .= deploymentStatusDescription
        , "target_url"     .= deploymentStatusTargetUrl
        , "created_at"     .= deploymentStatusCreatedAt
        , "updated_at"     .= deploymentStatusUpdatedAt
        , "deployment_url" .= deploymentStatusDeploymentUrl
        , "repository_url" .= deploymentStatusRepositoryUrl
        ]
