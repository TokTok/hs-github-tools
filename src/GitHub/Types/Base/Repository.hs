{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Repository where

import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              object)
import           Data.Aeson.Types            (Value (..), (.:), (.:?), (.=))
import           Data.Text                   (Text)
import           Test.QuickCheck.Arbitrary   (Arbitrary (..))

import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.License
import           GitHub.Types.Base.RepoOwner

------------------------------------------------------------------------------
-- Repository

data Repository = Repository
    { repositoryAllowAutoMerge      :: Maybe Bool
    , repositoryAllowForking        :: Bool
    , repositoryAllowMergeCommit    :: Maybe Bool
    , repositoryAllowRebaseMerge    :: Maybe Bool
    , repositoryAllowSquashMerge    :: Maybe Bool
    , repositoryAllowUpdateBranch   :: Maybe Bool
    , repositoryArchived            :: Bool
    , repositoryArchiveUrl          :: Text
    , repositoryAssigneesUrl        :: Text
    , repositoryBlobsUrl            :: Text
    , repositoryBranchesUrl         :: Text
    , repositoryCloneUrl            :: Text
    , repositoryCollaboratorsUrl    :: Text
    , repositoryCommentsUrl         :: Text
    , repositoryCommitsUrl          :: Text
    , repositoryCompareUrl          :: Text
    , repositoryContentsUrl         :: Text
    , repositoryContributorsUrl     :: Text
    , repositoryCreatedAt           :: DateTime
    , repositoryDefaultBranch       :: Text
    , repositoryDeleteBranchOnMerge :: Maybe Bool
    , repositoryDeploymentsUrl      :: Text
    , repositoryDescription         :: Maybe Text
    , repositoryDisabled            :: Bool
    , repositoryDownloadsUrl        :: Text
    , repositoryEventsUrl           :: Text
    , repositoryFork                :: Bool
    , repositoryForks               :: Int
    , repositoryForksCount          :: Int
    , repositoryForksUrl            :: Text
    , repositoryFullName            :: Text
    , repositoryGitCommitsUrl       :: Text
    , repositoryGitRefsUrl          :: Text
    , repositoryGitTagsUrl          :: Text
    , repositoryGitUrl              :: Text
    , repositoryIsTemplate          :: Bool
    , repositoryHasDownloads        :: Bool
    , repositoryHasIssues           :: Bool
    , repositoryHasPages            :: Bool
    , repositoryHasProjects         :: Bool
    , repositoryHasWiki             :: Bool
    , repositoryHomepage            :: Maybe Text
    , repositoryHooksUrl            :: Text
    , repositoryHtmlUrl             :: Text
    , repositoryId                  :: Int
    , repositoryIssueCommentUrl     :: Text
    , repositoryIssueEventsUrl      :: Text
    , repositoryIssuesUrl           :: Text
    , repositoryKeysUrl             :: Text
    , repositoryLabelsUrl           :: Text
    , repositoryLanguage            :: Maybe Text
    , repositoryLanguagesUrl        :: Text
    , repositoryLicense             :: Maybe License
    , repositoryMasterBranch        :: Maybe Text
    , repositoryMergesUrl           :: Text
    , repositoryMilestonesUrl       :: Text
    , repositoryMirrorUrl           :: Maybe Text
    , repositoryName                :: Text
    , repositoryNodeId              :: Text
    , repositoryNotificationsUrl    :: Text
    , repositoryOpenIssues          :: Int
    , repositoryOpenIssuesCount     :: Int
    , repositoryOrganization        :: Maybe Text
    , repositoryOwner               :: RepoOwner
    , repositoryPrivate             :: Bool
    , repositoryPublic              :: Maybe Bool
    , repositoryPullsUrl            :: Text
    , repositoryPushedAt            :: Maybe DateTime
    , repositoryReleasesUrl         :: Text
    , repositorySize                :: Int
    , repositorySshUrl              :: Text
    , repositoryStargazers          :: Maybe Int
    , repositoryStargazersCount     :: Int
    , repositoryStargazersUrl       :: Text
    , repositoryStatusesUrl         :: Text
    , repositorySubscribersUrl      :: Text
    , repositorySubscriptionUrl     :: Text
    , repositorySvnUrl              :: Text
    , repositoryTagsUrl             :: Text
    , repositoryTeamsUrl            :: Text
    , repositoryTopics              :: [Text]
    , repositoryTreesUrl            :: Text
    , repositoryUpdatedAt           :: DateTime
    , repositoryUrl                 :: Text
    , repositoryVisibility          :: Text
    , repositoryWatchers            :: Int
    , repositoryWatchersCount       :: Int
    } deriving (Eq, Show, Read)


instance FromJSON Repository where
    parseJSON (Object x) = Repository
        <$> x .:? "allow_auto_merge"
        <*> x .: "allow_forking"
        <*> x .:? "allow_merge_commit"
        <*> x .:? "allow_rebase_merge"
        <*> x .:? "allow_squash_merge"
        <*> x .:? "allow_update_branch"
        <*> x .: "archived"
        <*> x .: "archive_url"
        <*> x .: "assignees_url"
        <*> x .: "blobs_url"
        <*> x .: "branches_url"
        <*> x .: "clone_url"
        <*> x .: "collaborators_url"
        <*> x .: "comments_url"
        <*> x .: "commits_url"
        <*> x .: "compare_url"
        <*> x .: "contents_url"
        <*> x .: "contributors_url"
        <*> x .: "created_at"
        <*> x .: "default_branch"
        <*> x .:? "delete_branch_on_merge"
        <*> x .: "deployments_url"
        <*> x .: "description"
        <*> x .: "disabled"
        <*> x .: "downloads_url"
        <*> x .: "events_url"
        <*> x .: "fork"
        <*> x .: "forks"
        <*> x .: "forks_count"
        <*> x .: "forks_url"
        <*> x .: "full_name"
        <*> x .: "git_commits_url"
        <*> x .: "git_refs_url"
        <*> x .: "git_tags_url"
        <*> x .: "git_url"
        <*> x .: "is_template"
        <*> x .: "has_downloads"
        <*> x .: "has_issues"
        <*> x .: "has_pages"
        <*> x .: "has_projects"
        <*> x .: "has_wiki"
        <*> x .: "homepage"
        <*> x .: "hooks_url"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "issue_comment_url"
        <*> x .: "issue_events_url"
        <*> x .: "issues_url"
        <*> x .: "keys_url"
        <*> x .: "labels_url"
        <*> x .:? "language"
        <*> x .: "languages_url"
        <*> x .: "license"
        <*> x .:? "master_branch"
        <*> x .: "merges_url"
        <*> x .: "milestones_url"
        <*> x .: "mirror_url"
        <*> x .: "name"
        <*> x .: "node_id"
        <*> x .: "notifications_url"
        <*> x .: "open_issues"
        <*> x .: "open_issues_count"
        <*> x .:? "organization"
        <*> x .: "owner"
        <*> x .: "private"
        <*> x .:? "public"
        <*> x .: "pulls_url"
        <*> x .: "pushed_at"
        <*> x .: "releases_url"
        <*> x .: "size"
        <*> x .: "ssh_url"
        <*> x .:? "stargazers"
        <*> x .: "stargazers_count"
        <*> x .: "stargazers_url"
        <*> x .: "statuses_url"
        <*> x .: "subscribers_url"
        <*> x .: "subscription_url"
        <*> x .: "svn_url"
        <*> x .: "tags_url"
        <*> x .: "teams_url"
        <*> x .: "topics"
        <*> x .: "trees_url"
        <*> x .: "updated_at"
        <*> x .: "url"
        <*> x .: "visibility"
        <*> x .: "watchers"
        <*> x .: "watchers_count"

    parseJSON _ = fail "Repository"

instance ToJSON Repository where
    toJSON Repository{..} = object
        [ "allow_auto_merge"       .= repositoryAllowAutoMerge
        , "allow_forking"          .= repositoryAllowForking
        , "allow_merge_commit"     .= repositoryAllowMergeCommit
        , "allow_rebase_merge"     .= repositoryAllowRebaseMerge
        , "allow_squash_merge"     .= repositoryAllowSquashMerge
        , "allow_update_branch"    .= repositoryAllowUpdateBranch
        , "archived"               .= repositoryArchived
        , "archive_url"            .= repositoryArchiveUrl
        , "assignees_url"          .= repositoryAssigneesUrl
        , "blobs_url"              .= repositoryBlobsUrl
        , "branches_url"           .= repositoryBranchesUrl
        , "clone_url"              .= repositoryCloneUrl
        , "collaborators_url"      .= repositoryCollaboratorsUrl
        , "comments_url"           .= repositoryCommentsUrl
        , "commits_url"            .= repositoryCommitsUrl
        , "compare_url"            .= repositoryCompareUrl
        , "contents_url"           .= repositoryContentsUrl
        , "contributors_url"       .= repositoryContributorsUrl
        , "created_at"             .= repositoryCreatedAt
        , "default_branch"         .= repositoryDefaultBranch
        , "delete_branch_on_merge" .= repositoryDeleteBranchOnMerge
        , "deployments_url"        .= repositoryDeploymentsUrl
        , "description"            .= repositoryDescription
        , "disabled"               .= repositoryDisabled
        , "downloads_url"          .= repositoryDownloadsUrl
        , "events_url"             .= repositoryEventsUrl
        , "fork"                   .= repositoryFork
        , "forks"                  .= repositoryForks
        , "forks_count"            .= repositoryForksCount
        , "forks_url"              .= repositoryForksUrl
        , "full_name"              .= repositoryFullName
        , "git_commits_url"        .= repositoryGitCommitsUrl
        , "git_refs_url"           .= repositoryGitRefsUrl
        , "git_tags_url"           .= repositoryGitTagsUrl
        , "git_url"                .= repositoryGitUrl
        , "is_template"            .= repositoryIsTemplate
        , "has_downloads"          .= repositoryHasDownloads
        , "has_issues"             .= repositoryHasIssues
        , "has_pages"              .= repositoryHasPages
        , "has_projects"           .= repositoryHasProjects
        , "has_wiki"               .= repositoryHasWiki
        , "homepage"               .= repositoryHomepage
        , "hooks_url"              .= repositoryHooksUrl
        , "html_url"               .= repositoryHtmlUrl
        , "id"                     .= repositoryId
        , "issue_comment_url"      .= repositoryIssueCommentUrl
        , "issue_events_url"       .= repositoryIssueEventsUrl
        , "issues_url"             .= repositoryIssuesUrl
        , "keys_url"               .= repositoryKeysUrl
        , "labels_url"             .= repositoryLabelsUrl
        , "language"               .= repositoryLanguage
        , "languages_url"          .= repositoryLanguagesUrl
        , "license"                .= repositoryLicense
        , "master_branch"          .= repositoryMasterBranch
        , "merges_url"             .= repositoryMergesUrl
        , "milestones_url"         .= repositoryMilestonesUrl
        , "mirror_url"             .= repositoryMirrorUrl
        , "name"                   .= repositoryName
        , "node_id"                .= repositoryNodeId
        , "notifications_url"      .= repositoryNotificationsUrl
        , "open_issues"            .= repositoryOpenIssues
        , "open_issues_count"      .= repositoryOpenIssuesCount
        , "organization"           .= repositoryOrganization
        , "owner"                  .= repositoryOwner
        , "private"                .= repositoryPrivate
        , "public"                 .= repositoryPublic
        , "pulls_url"              .= repositoryPullsUrl
        , "pushed_at"              .= repositoryPushedAt
        , "releases_url"           .= repositoryReleasesUrl
        , "size"                   .= repositorySize
        , "ssh_url"                .= repositorySshUrl
        , "stargazers"             .= repositoryStargazers
        , "stargazers_count"       .= repositoryStargazersCount
        , "stargazers_url"         .= repositoryStargazersUrl
        , "statuses_url"           .= repositoryStatusesUrl
        , "subscribers_url"        .= repositorySubscribersUrl
        , "subscription_url"       .= repositorySubscriptionUrl
        , "svn_url"                .= repositorySvnUrl
        , "tags_url"               .= repositoryTagsUrl
        , "teams_url"              .= repositoryTeamsUrl
        , "topics"                 .= repositoryTopics
        , "trees_url"              .= repositoryTreesUrl
        , "updated_at"             .= repositoryUpdatedAt
        , "url"                    .= repositoryUrl
        , "visibility"             .= repositoryVisibility
        , "watchers"               .= repositoryWatchers
        , "watchers_count"         .= repositoryWatchersCount
        ]


instance Arbitrary Repository where
    arbitrary = Repository
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
