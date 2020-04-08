{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.Release where

import           Control.Applicative        ((<$>), (<*>))
import           Data.Aeson                 (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types           (Value (..), (.:), (.=))
import           Data.Text                  (Text)
import           Test.QuickCheck.Arbitrary  (Arbitrary (..))

import           GitHub.Types.Base.DateTime
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- Release

data Release = Release
    { releaseAssets          :: [Text]
    , releaseAssetsUrl       :: Text
    , releaseAuthor          :: User
    , releaseBody            :: Text
    , releaseCreatedAt       :: DateTime
    , releaseDraft           :: Bool
    , releaseHtmlUrl         :: Text
    , releaseId              :: Int
    , releaseName            :: Text
    , releaseNodeId          :: Text
    , releasePrerelease      :: Bool
    , releasePublishedAt     :: DateTime
    , releaseTagName         :: Text
    , releaseTarballUrl      :: Maybe Text
    , releaseTargetCommitish :: Text
    , releaseUploadUrl       :: Text
    , releaseUrl             :: Text
    , releaseZipballUrl      :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Release where
    parseJSON (Object x) = Release
        <$> x .: "assets"
        <*> x .: "assets_url"
        <*> x .: "author"
        <*> x .: "body"
        <*> x .: "created_at"
        <*> x .: "draft"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "name"
        <*> x .: "node_id"
        <*> x .: "prerelease"
        <*> x .: "published_at"
        <*> x .: "tag_name"
        <*> x .: "tarball_url"
        <*> x .: "target_commitish"
        <*> x .: "upload_url"
        <*> x .: "url"
        <*> x .: "zipball_url"

    parseJSON _ = fail "Release"


instance ToJSON Release where
    toJSON Release{..} = object
        [ "assets"           .= releaseAssets
        , "assets_url"       .= releaseAssetsUrl
        , "author"           .= releaseAuthor
        , "body"             .= releaseBody
        , "created_at"       .= releaseCreatedAt
        , "draft"            .= releaseDraft
        , "html_url"         .= releaseHtmlUrl
        , "id"               .= releaseId
        , "name"             .= releaseName
        , "node_id"          .= releaseNodeId
        , "prerelease"       .= releasePrerelease
        , "published_at"     .= releasePublishedAt
        , "tag_name"         .= releaseTagName
        , "tarball_url"      .= releaseTarballUrl
        , "target_commitish" .= releaseTargetCommitish
        , "upload_url"       .= releaseUploadUrl
        , "url"              .= releaseUrl
        , "zipball_url"      .= releaseZipballUrl
        ]


instance Arbitrary Release where
    arbitrary = Release
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
