{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Team where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Team

data Team = Team
    { teamDescription     :: Text
    , teamId              :: Int
    , teamHtmlUrl         :: Text
    , teamMembersUrl      :: Text
    , teamName            :: Text
    , teamNodeId          :: Text
    , teamParent          :: Maybe Team
    , teamPermission      :: Text
    , teamPrivacy         :: Text
    , teamRepositoriesUrl :: Text
    , teamSlug            :: Text
    , teamUrl             :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Team where
    parseJSON (Object x) = Team
        <$> x .: "description"
        <*> x .: "id"
        <*> x .: "html_url"
        <*> x .: "members_url"
        <*> x .: "name"
        <*> x .: "node_id"
        <*> x .:? "parent"
        <*> x .: "permission"
        <*> x .: "privacy"
        <*> x .: "repositories_url"
        <*> x .: "slug"
        <*> x .: "url"

    parseJSON _ = fail "Team"


instance ToJSON Team where
    toJSON Team{..} = object
        [ "description"      .= teamDescription
        , "id"               .= teamId
        , "html_url"         .= teamHtmlUrl
        , "members_url"      .= teamMembersUrl
        , "name"             .= teamName
        , "node_id"          .= teamNodeId
        , "parent"           .= teamParent
        , "permission"       .= teamPermission
        , "privacy"          .= teamPrivacy
        , "repositories_url" .= teamRepositoriesUrl
        , "slug"             .= teamSlug
        , "url"              .= teamUrl
        ]


instance Arbitrary Team where
    arbitrary = Team
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
