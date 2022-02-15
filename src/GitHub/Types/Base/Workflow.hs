{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Workflow where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Workflow

data Workflow = Workflow
    { workflowBadgeUrl  :: Text
    , workflowCreatedAt :: Text
    , workflowHtmlUrl   :: Text
    , workflowId        :: Int
    , workflowName      :: Text
    , workflowNodeId    :: Text
    , workflowPath      :: Text
    , workflowState     :: Text
    , workflowUpdatedAt :: Text
    , workflowUrl       :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Workflow where
    parseJSON (Object x) = Workflow
        <$> x .: "badge_url"
        <*> x .: "created_at"
        <*> x .: "html_url"
        <*> x .: "id"
        <*> x .: "name"
        <*> x .: "node_id"
        <*> x .: "path"
        <*> x .: "state"
        <*> x .: "updated_at"
        <*> x .: "url"

    parseJSON _ = fail "Workflow"


instance ToJSON Workflow where
    toJSON Workflow{..} = object
        [ "badge_url"  .= workflowBadgeUrl
        , "created_at" .= workflowCreatedAt
        , "html_url"   .= workflowHtmlUrl
        , "id"         .= workflowId
        , "name"       .= workflowName
        , "node_id"    .= workflowNodeId
        , "path"       .= workflowPath
        , "state"      .= workflowState
        , "updated_at" .= workflowUpdatedAt
        , "url"        .= workflowUrl
        ]


instance Arbitrary Workflow where
    arbitrary = Workflow
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
