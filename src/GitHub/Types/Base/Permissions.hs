{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.Permissions where

import           Control.Applicative       ((<$>), (<*>))
import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:?), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Permissions

data Permissions = Permissions
    { permissionsChecks            :: Maybe Text
    , permissionsContents          :: Maybe Text
    , permissionsContentReferences :: Maybe Text
    , permissionsDeployments       :: Maybe Text
    , permissionsIssues            :: Maybe Text
    , permissionsMembers           :: Maybe Text
    , permissionsMetadata          :: Maybe Text
    , permissionsPages             :: Maybe Text
    , permissionsPullRequests      :: Maybe Text
    , permissionsRepositoryHooks   :: Maybe Text
    , permissionsSingleFile        :: Maybe Text
    , permissionsStatuses          :: Maybe Text
    } deriving (Eq, Show, Read)


instance FromJSON Permissions where
    parseJSON (Object x) = Permissions
        <$> x .:? "checks"
        <*> x .:? "contents"
        <*> x .:? "content_references"
        <*> x .:? "deployments"
        <*> x .:? "issues"
        <*> x .:? "members"
        <*> x .:? "metadata"
        <*> x .:? "pages"
        <*> x .:? "pull_requests"
        <*> x .:? "repository_hooks"
        <*> x .:? "single_file"
        <*> x .:? "statuses"

    parseJSON _ = fail "Permissions"


instance ToJSON Permissions where
    toJSON Permissions{..} = object
        [ "checks"             .= permissionsChecks
        , "contents"           .= permissionsContents
        , "content_references" .= permissionsContentReferences
        , "deployments"        .= permissionsDeployments
        , "issues"             .= permissionsIssues
        , "members"            .= permissionsMembers
        , "metadata"           .= permissionsMetadata
        , "pages"              .= permissionsPages
        , "pull_requests"      .= permissionsPullRequests
        , "repository_hooks"   .= permissionsRepositoryHooks
        , "single_file"        .= permissionsSingleFile
        , "statuses"           .= permissionsStatuses
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
