{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.CheckApp where

import           Control.Applicative           ((<$>), (<*>))
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                object)
import           Data.Aeson.Types              (Value (..), (.:), (.=))
import           Data.Text                     (Text)
import           Test.QuickCheck.Arbitrary     (Arbitrary (..))

import           GitHub.Types.Base.Permissions
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- CheckApp

data CheckApp = CheckApp
    { checkAppCreatedAt   :: Text
    , checkAppDescription :: Text
    , checkAppEvents      :: [Text]
    , checkAppExternalUrl :: Text
    , checkAppHtmlUrl     :: Text
    , checkAppId          :: Int
    , checkAppName        :: Text
    , checkAppNodeId      :: Text
    , checkAppOwner       :: User
    , checkAppPermissions :: Permissions
    , checkAppSlug        :: Text
    , checkAppUpdatedAt   :: Text
    } deriving (Eq, Show, Read)


instance FromJSON CheckApp where
    parseJSON (Object x) = CheckApp
        <$> x .: "created_at"
        <*> x .: "description"
        <*> x .: "events"
        <*> x .: "external_url"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "name"
        <*> x .: "node_id"
        <*> x .: "owner"
        <*> x .: "permissions"
        <*> x .: "slug"
        <*> x .: "updated_at"

    parseJSON _ = fail "CheckApp"


instance ToJSON CheckApp where
    toJSON CheckApp{..} = object
        [ "created_at"   .= checkAppCreatedAt
        , "description"  .= checkAppDescription
        , "events"       .= checkAppEvents
        , "external_url" .= checkAppExternalUrl
        , "html_url"     .= checkAppHtmlUrl
        , "id"           .= checkAppId
        , "name"         .= checkAppName
        , "node_id"      .= checkAppNodeId
        , "owner"        .= checkAppOwner
        , "permissions"  .= checkAppPermissions
        , "slug"         .= checkAppSlug
        , "updated_at"   .= checkAppUpdatedAt
        ]


instance Arbitrary CheckApp where
    arbitrary = CheckApp
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
