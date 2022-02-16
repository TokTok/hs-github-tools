{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Issue where

import           Data.Aeson                       (FromJSON (..), ToJSON (..),
                                                   object)
import           Data.Aeson.Types                 (Value (..), (.:), (.:?),
                                                   (.=))
import           Data.Text                        (Text)
import           Test.QuickCheck.Arbitrary        (Arbitrary (..))

import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.Label
import           GitHub.Types.Base.Milestone
import           GitHub.Types.Base.PullRequestRef
import           GitHub.Types.Base.Reactions
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- Issue

data Issue = Issue
    { issueActiveLockReason      :: Maybe Text
    , issueAssignee              :: Maybe User
    , issueAssignees             :: [User]
    , issueAuthorAssociation     :: Text
    , issueBody                  :: Maybe Text
    , issueClosedAt              :: Maybe DateTime
    , issueComments              :: Int
    , issueCommentsUrl           :: Text
    , issueCreatedAt             :: DateTime
    , issueDraft                 :: Maybe Bool
    , issueEventsUrl             :: Text
    , issueHtmlUrl               :: Text
    , issueId                    :: Int
    , issueLabels                :: [Label]
    , issueLabelsUrl             :: Text
    , issueLocked                :: Bool
    , issueMilestone             :: Maybe Milestone
    , issueNodeId                :: Text
    , issueNumber                :: Int
    , issuePerformedViaGithubApp :: Maybe Text
    , issuePullRequest           :: Maybe PullRequestRef
    , issueReactions             :: Reactions
    , issueRepositoryUrl         :: Text
    , issueState                 :: Text
    , issueTimelineUrl           :: Text
    , issueTitle                 :: Text
    , issueUpdatedAt             :: DateTime
    , issueUrl                   :: Text
    , issueUser                  :: User
    } deriving (Eq, Show, Read)


instance FromJSON Issue where
    parseJSON (Object x) = Issue
        <$> x .: "active_lock_reason"
        <*> x .: "assignee"
        <*> x .: "assignees"
        <*> x .: "author_association"
        <*> x .: "body"
        <*> x .: "closed_at"
        <*> x .: "comments"
        <*> x .: "comments_url"
        <*> x .: "created_at"
        <*> x .: "draft"
        <*> x .: "events_url"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "labels"
        <*> x .: "labels_url"
        <*> x .: "locked"
        <*> x .: "milestone"
        <*> x .: "node_id"
        <*> x .: "number"
        <*> x .: "performed_via_github_app"
        <*> x .:? "pull_request"
        <*> x .: "reactions"
        <*> x .: "repository_url"
        <*> x .: "state"
        <*> x .: "timeline_url"
        <*> x .: "title"
        <*> x .: "updated_at"
        <*> x .: "url"
        <*> x .: "user"

    parseJSON _ = fail "Issue"


instance ToJSON Issue where
    toJSON Issue{..} = object
        [ "active_lock_reason"       .= issueActiveLockReason
        , "assignee"                 .= issueAssignee
        , "assignees"                .= issueAssignees
        , "author_association"       .= issueAuthorAssociation
        , "body"                     .= issueBody
        , "closed_at"                .= issueClosedAt
        , "comments"                 .= issueComments
        , "comments_url"             .= issueCommentsUrl
        , "created_at"               .= issueCreatedAt
        , "draft"                    .= issueDraft
        , "events_url"               .= issueEventsUrl
        , "html_url"                 .= issueHtmlUrl
        , "id"                       .= issueId
        , "labels"                   .= issueLabels
        , "labels_url"               .= issueLabelsUrl
        , "locked"                   .= issueLocked
        , "milestone"                .= issueMilestone
        , "node_id"                  .= issueNodeId
        , "number"                   .= issueNumber
        , "performed_via_github_app" .= issuePerformedViaGithubApp
        , "pull_request"             .= issuePullRequest
        , "reactions"                .= issueReactions
        , "repository_url"           .= issueRepositoryUrl
        , "state"                    .= issueState
        , "timeline_url"             .= issueTimelineUrl
        , "title"                    .= issueTitle
        , "updated_at"               .= issueUpdatedAt
        , "url"                      .= issueUrl
        , "user"                     .= issueUser
        ]


instance Arbitrary Issue where
    arbitrary = Issue
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
