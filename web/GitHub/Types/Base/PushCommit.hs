{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.PushCommit where

import           Control.Applicative      ((<$>), (<*>))
import           Data.Aeson               (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types         (Value (..), (.:), (.=))
import           Data.Text                (Text)

import           GitHub.Types.Base.Author

------------------------------------------------------------------------------
-- PushCommit

data PushCommit = PushCommit
    { pushCommitId        :: Text
    , pushCommitTreeId    :: Text
    , pushCommitDistinct  :: Bool
    , pushCommitMessage   :: Text
    , pushCommitTimestamp :: Text
    , pushCommitUrl       :: Text
    , pushCommitAuthor    :: Author
    , pushCommitCommitter :: Author
    , pushCommitAdded     :: [Text]
    , pushCommitRemoved   :: [Text]
    , pushCommitModified  :: [Text]
    } deriving (Eq, Show, Read)


instance FromJSON PushCommit where
    parseJSON (Object x) = PushCommit
        <$> x .: "id"
        <*> x .: "tree_id"
        <*> x .: "distinct"
        <*> x .: "message"
        <*> x .: "timestamp"
        <*> x .: "url"
        <*> x .: "author"
        <*> x .: "committer"
        <*> x .: "added"
        <*> x .: "removed"
        <*> x .: "modified"

    parseJSON _ = fail "PushCommit"


instance ToJSON PushCommit where
    toJSON PushCommit{..} = object
        [ "id"           .= pushCommitId
        , "tree_id"      .= pushCommitTreeId
        , "distinct"     .= pushCommitDistinct
        , "message"      .= pushCommitMessage
        , "timestamp"    .= pushCommitTimestamp
        , "url"          .= pushCommitUrl
        , "author"       .= pushCommitAuthor
        , "committer"    .= pushCommitCommitter
        , "added"        .= pushCommitAdded
        , "removed"      .= pushCommitRemoved
        , "modified"     .= pushCommitModified
        ]
