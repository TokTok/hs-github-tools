{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.SimpleRepository where

import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              object)
import           Data.Aeson.Types            (Value (..), (.:), (.=))
import           Data.Text                   (Text)
import           Test.QuickCheck.Arbitrary   (Arbitrary (..))

import           GitHub.Types.Base.RepoOwner

------------------------------------------------------------------------------
-- SimpleRepository

data SimpleRepository = SimpleRepository
    { simpleRepositoryArchiveUrl       :: Text
    , simpleRepositoryAssigneesUrl     :: Text
    , simpleRepositoryBlobsUrl         :: Text
    , simpleRepositoryBranchesUrl      :: Text
    , simpleRepositoryCollaboratorsUrl :: Text
    , simpleRepositoryCommentsUrl      :: Text
    , simpleRepositoryCommitsUrl       :: Text
    , simpleRepositoryCompareUrl       :: Text
    , simpleRepositoryContentsUrl      :: Text
    , simpleRepositoryContributorsUrl  :: Text
    , simpleRepositoryDeploymentsUrl   :: Text
    , simpleRepositoryDescription      :: Maybe Text
    , simpleRepositoryDownloadsUrl     :: Text
    , simpleRepositoryEventsUrl        :: Text
    , simpleRepositoryFork             :: Bool
    , simpleRepositoryForksUrl         :: Text
    , simpleRepositoryFullName         :: Text
    , simpleRepositoryGitCommitsUrl    :: Text
    , simpleRepositoryGitRefsUrl       :: Text
    , simpleRepositoryGitTagsUrl       :: Text
    , simpleRepositoryHooksUrl         :: Text
    , simpleRepositoryHtmlUrl          :: Text
    , simpleRepositoryId               :: Int
    , simpleRepositoryIssueCommentUrl  :: Text
    , simpleRepositoryIssueEventsUrl   :: Text
    , simpleRepositoryIssuesUrl        :: Text
    , simpleRepositoryKeysUrl          :: Text
    , simpleRepositoryLabelsUrl        :: Text
    , simpleRepositoryLanguagesUrl     :: Text
    , simpleRepositoryMergesUrl        :: Text
    , simpleRepositoryMilestonesUrl    :: Text
    , simpleRepositoryName             :: Text
    , simpleRepositoryNodeId           :: Text
    , simpleRepositoryNotificationsUrl :: Text
    , simpleRepositoryOwner            :: RepoOwner
    , simpleRepositoryPrivate          :: Bool
    , simpleRepositoryPullsUrl         :: Text
    , simpleRepositoryReleasesUrl      :: Text
    , simpleRepositoryStargazersUrl    :: Text
    , simpleRepositoryStatusesUrl      :: Text
    , simpleRepositorySubscribersUrl   :: Text
    , simpleRepositorySubscriptionUrl  :: Text
    , simpleRepositoryTagsUrl          :: Text
    , simpleRepositoryTeamsUrl         :: Text
    , simpleRepositoryTreesUrl         :: Text
    , simpleRepositoryUrl              :: Text
    } deriving (Eq, Show, Read)


instance FromJSON SimpleRepository where
    parseJSON (Object x) = SimpleRepository
        <$> x .: "archive_url"
        <*> x .: "assignees_url"
        <*> x .: "blobs_url"
        <*> x .: "branches_url"
        <*> x .: "collaborators_url"
        <*> x .: "comments_url"
        <*> x .: "commits_url"
        <*> x .: "compare_url"
        <*> x .: "contents_url"
        <*> x .: "contributors_url"
        <*> x .: "deployments_url"
        <*> x .: "description"
        <*> x .: "downloads_url"
        <*> x .: "events_url"
        <*> x .: "fork"
        <*> x .: "forks_url"
        <*> x .: "full_name"
        <*> x .: "git_commits_url"
        <*> x .: "git_refs_url"
        <*> x .: "git_tags_url"
        <*> x .: "hooks_url"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "issue_comment_url"
        <*> x .: "issue_events_url"
        <*> x .: "issues_url"
        <*> x .: "keys_url"
        <*> x .: "labels_url"
        <*> x .: "languages_url"
        <*> x .: "merges_url"
        <*> x .: "milestones_url"
        <*> x .: "name"
        <*> x .: "node_id"
        <*> x .: "notifications_url"
        <*> x .: "owner"
        <*> x .: "private"
        <*> x .: "pulls_url"
        <*> x .: "releases_url"
        <*> x .: "stargazers_url"
        <*> x .: "statuses_url"
        <*> x .: "subscribers_url"
        <*> x .: "subscription_url"
        <*> x .: "tags_url"
        <*> x .: "teams_url"
        <*> x .: "trees_url"
        <*> x .: "url"

    parseJSON _ = fail "SimpleRepository"

instance ToJSON SimpleRepository where
    toJSON SimpleRepository{..} = object
        [ "archive_url"       .= simpleRepositoryArchiveUrl
        , "assignees_url"     .= simpleRepositoryAssigneesUrl
        , "blobs_url"         .= simpleRepositoryBlobsUrl
        , "branches_url"      .= simpleRepositoryBranchesUrl
        , "collaborators_url" .= simpleRepositoryCollaboratorsUrl
        , "comments_url"      .= simpleRepositoryCommentsUrl
        , "commits_url"       .= simpleRepositoryCommitsUrl
        , "compare_url"       .= simpleRepositoryCompareUrl
        , "contents_url"      .= simpleRepositoryContentsUrl
        , "contributors_url"  .= simpleRepositoryContributorsUrl
        , "deployments_url"   .= simpleRepositoryDeploymentsUrl
        , "description"       .= simpleRepositoryDescription
        , "downloads_url"     .= simpleRepositoryDownloadsUrl
        , "events_url"        .= simpleRepositoryEventsUrl
        , "fork"              .= simpleRepositoryFork
        , "forks_url"         .= simpleRepositoryForksUrl
        , "full_name"         .= simpleRepositoryFullName
        , "git_commits_url"   .= simpleRepositoryGitCommitsUrl
        , "git_refs_url"      .= simpleRepositoryGitRefsUrl
        , "git_tags_url"      .= simpleRepositoryGitTagsUrl
        , "hooks_url"         .= simpleRepositoryHooksUrl
        , "html_url"          .= simpleRepositoryHtmlUrl
        , "id"                .= simpleRepositoryId
        , "issue_comment_url" .= simpleRepositoryIssueCommentUrl
        , "issue_events_url"  .= simpleRepositoryIssueEventsUrl
        , "issues_url"        .= simpleRepositoryIssuesUrl
        , "keys_url"          .= simpleRepositoryKeysUrl
        , "labels_url"        .= simpleRepositoryLabelsUrl
        , "languages_url"     .= simpleRepositoryLanguagesUrl
        , "merges_url"        .= simpleRepositoryMergesUrl
        , "milestones_url"    .= simpleRepositoryMilestonesUrl
        , "name"              .= simpleRepositoryName
        , "node_id"           .= simpleRepositoryNodeId
        , "notifications_url" .= simpleRepositoryNotificationsUrl
        , "owner"             .= simpleRepositoryOwner
        , "private"           .= simpleRepositoryPrivate
        , "pulls_url"         .= simpleRepositoryPullsUrl
        , "releases_url"      .= simpleRepositoryReleasesUrl
        , "stargazers_url"    .= simpleRepositoryStargazersUrl
        , "statuses_url"      .= simpleRepositoryStatusesUrl
        , "subscribers_url"   .= simpleRepositorySubscribersUrl
        , "subscription_url"  .= simpleRepositorySubscriptionUrl
        , "tags_url"          .= simpleRepositoryTagsUrl
        , "teams_url"         .= simpleRepositoryTeamsUrl
        , "trees_url"         .= simpleRepositoryTreesUrl
        , "url"               .= simpleRepositoryUrl
        ]


instance Arbitrary SimpleRepository where
    arbitrary = SimpleRepository
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
