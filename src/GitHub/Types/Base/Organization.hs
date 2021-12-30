{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Organization where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Organization

data Organization = Organization
    { organizationAvatarUrl        :: Text
    , organizationDescription      :: Text
    , organizationEmail            :: Maybe Text
    , organizationEventsUrl        :: Text
    , organizationHooksUrl         :: Text
    , organizationId               :: Int
    , organizationIssuesUrl        :: Text
    , organizationLogin            :: Text
    , organizationMembersUrl       :: Text
    , organizationNodeId           :: Text
    , organizationPublicMembersUrl :: Text
    , organizationReposUrl         :: Text
    , organizationUrl              :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Organization where
    parseJSON (Object x) = Organization
        <$> x .: "avatar_url"
        <*> x .: "description"
        <*> x .:? "email"
        <*> x .: "events_url"
        <*> x .: "hooks_url"
        <*> x .: "id"
        <*> x .: "issues_url"
        <*> x .: "login"
        <*> x .: "members_url"
        <*> x .: "node_id"
        <*> x .: "public_members_url"
        <*> x .: "repos_url"
        <*> x .: "url"

    parseJSON _ = fail "Organization"


instance ToJSON Organization where
    toJSON Organization{..} = object
        [ "avatar_url"         .= organizationAvatarUrl
        , "description"        .= organizationDescription
        , "email"              .= organizationEmail
        , "events_url"         .= organizationEventsUrl
        , "hooks_url"          .= organizationHooksUrl
        , "id"                 .= organizationId
        , "issues_url"         .= organizationIssuesUrl
        , "login"              .= organizationLogin
        , "members_url"        .= organizationMembersUrl
        , "node_id"            .= organizationNodeId
        , "public_members_url" .= organizationPublicMembersUrl
        , "repos_url"          .= organizationReposUrl
        , "url"                .= organizationUrl
        ]


instance Arbitrary Organization where
    arbitrary = Organization
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
