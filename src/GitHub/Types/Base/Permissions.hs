{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Permissions where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:?), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Permissions

data Permissions = Permissions
    { permissionsActions                      :: Maybe Text
    , permissionsAdministration               :: Maybe Text
    , permissionsAttestations                 :: Maybe Text
    , permissionsChecks                       :: Maybe Text
    , permissionsContents                     :: Maybe Text
    , permissionsContentReferences            :: Maybe Text
    , permissionsDeployments                  :: Maybe Text
    , permissionsDiscussions                  :: Maybe Text
    , permissionsEmails                       :: Maybe Text
    , permissionsIssues                       :: Maybe Text
    , permissionsKeys                         :: Maybe Text
    , permissionsMembers                      :: Maybe Text
    , permissionsMergeQueues                  :: Maybe Text
    , permissionsMetadata                     :: Maybe Text
    , permissionsOrganizationAdministration   :: Maybe Text
    , permissionsOrganizationCustomProperties :: Maybe Text
    , permissionsOrganizationHooks            :: Maybe Text
    , permissionsOrganizationPackages         :: Maybe Text
    , permissionsOrganizationProjects         :: Maybe Text
    , permissionsPackages                     :: Maybe Text
    , permissionsPages                        :: Maybe Text
    , permissionsPullRequests                 :: Maybe Text
    , permissionsRepositoryCustomProperties   :: Maybe Text
    , permissionsRepositoryHooks              :: Maybe Text
    , permissionsRepositoryProjects           :: Maybe Text
    , permissionsSecurityEvents               :: Maybe Text
    , permissionsSingleFile                   :: Maybe Text
    , permissionsStatuses                     :: Maybe Text
    , permissionsVulnerabilityAlerts          :: Maybe Text
    } deriving (Eq, Show, Read)


instance FromJSON Permissions where
    parseJSON (Object x) = Permissions
        <$> x .:? "actions"
        <*> x .:? "administration"
        <*> x .:? "attestations"
        <*> x .:? "checks"
        <*> x .:? "contents"
        <*> x .:? "content_references"
        <*> x .:? "deployments"
        <*> x .:? "discussions"
        <*> x .:? "emails"
        <*> x .:? "issues"
        <*> x .:? "keys"
        <*> x .:? "members"
        <*> x .:? "merge_queues"
        <*> x .:? "metadata"
        <*> x .:? "organization_administration"
        <*> x .:? "organization_custom_properties"
        <*> x .:? "organization_hooks"
        <*> x .:? "organization_packages"
        <*> x .:? "organization_projects"
        <*> x .:? "packages"
        <*> x .:? "pages"
        <*> x .:? "pull_requests"
        <*> x .:? "repository_custom_properties"
        <*> x .:? "repository_hooks"
        <*> x .:? "repository_projects"
        <*> x .:? "security_events"
        <*> x .:? "single_file"
        <*> x .:? "statuses"
        <*> x .:? "vulnerability_alerts"

    parseJSON _ = fail "Permissions"


instance ToJSON Permissions where
    toJSON Permissions{..} = object
        [ "actions"                        .= permissionsActions
        , "administration"                 .= permissionsAdministration
        , "attestations"                   .= permissionsAttestations
        , "checks"                         .= permissionsChecks
        , "contents"                       .= permissionsContents
        , "content_references"             .= permissionsContentReferences
        , "deployments"                    .= permissionsDeployments
        , "discussions"                    .= permissionsDiscussions
        , "emails"                         .= permissionsEmails
        , "issues"                         .= permissionsIssues
        , "keys"                           .= permissionsKeys
        , "members"                        .= permissionsMembers
        , "merge_queues"                   .= permissionsMergeQueues
        , "metadata"                       .= permissionsMetadata
        , "organization_administration"    .= permissionsOrganizationAdministration
        , "organization_custom_properties" .= permissionsOrganizationCustomProperties
        , "organization_hooks"             .= permissionsOrganizationHooks
        , "organization_packages"          .= permissionsOrganizationPackages
        , "organization_projects"          .= permissionsOrganizationProjects
        , "packages"                       .= permissionsPackages
        , "pages"                          .= permissionsPages
        , "pull_requests"                  .= permissionsPullRequests
        , "repository_custom_properties"   .= permissionsRepositoryCustomProperties
        , "repository_hooks"               .= permissionsRepositoryHooks
        , "repository_projects"            .= permissionsRepositoryProjects
        , "security_events"                .= permissionsSecurityEvents
        , "single_file"                    .= permissionsSingleFile
        , "statuses"                       .= permissionsStatuses
        , "vulnerability_alerts"           .= permissionsVulnerabilityAlerts
        ]


instance Arbitrary Permissions where
    arbitrary = Permissions
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
