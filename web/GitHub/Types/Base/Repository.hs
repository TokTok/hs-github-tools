{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.Repository where

import           Control.Applicative         ((<$>), (<*>))
import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              object)
import           Data.Aeson.Types            (Value (..), (.:), (.:?), (.=))
import           Data.Text                   (Text)

import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.RepoOwner

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
