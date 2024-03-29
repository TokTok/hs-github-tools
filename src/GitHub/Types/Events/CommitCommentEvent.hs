{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Events.CommitCommentEvent where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.:?), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base
import           GitHub.Types.Event


data CommitCommentEvent = CommitCommentEvent
    { commitCommentEventInstallation :: Maybe Installation
    , commitCommentEventOrganization :: Organization
    , commitCommentEventRepository   :: Repository
    , commitCommentEventSender       :: User

    , commitCommentEventAction       :: Text
    , commitCommentEventComment      :: CommitComment
    } deriving (Eq, Show, Read)

instance Event CommitCommentEvent where
    typeName = TypeName "CommitCommentEvent"
    eventName = EventName "commit_comment"

instance FromJSON CommitCommentEvent where
    parseJSON (Object x) = CommitCommentEvent
        <$> x .:? "installation"
        <*> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "comment"

    parseJSON _ = fail "CommitCommentEvent"

instance ToJSON CommitCommentEvent where
    toJSON CommitCommentEvent{..} = object
        [ "installation" .= commitCommentEventInstallation
        , "organization" .= commitCommentEventOrganization
        , "repository"   .= commitCommentEventRepository
        , "sender"       .= commitCommentEventSender

        , "action"       .= commitCommentEventAction
        , "comment"      .= commitCommentEventComment
        ]


instance Arbitrary CommitCommentEvent where
    arbitrary = CommitCommentEvent
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

        <*> arbitrary
        <*> arbitrary
