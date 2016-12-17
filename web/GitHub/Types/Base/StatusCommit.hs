{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.StatusCommit where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  object)
import           Data.Aeson.Types                (Value (..), (.:), (.=))
import           Data.Text                       (Text)

import           GitHub.Types.Base.CommitDetails
import           GitHub.Types.Base.CommitRefHtml
import           GitHub.Types.Base.User

------------------------------------------------------------------------------
-- StatusCommit

data StatusCommit = StatusCommit
    { statusCommitSha         :: Text
    , statusCommitCommit      :: CommitDetails
    , statusCommitUrl         :: Text
    , statusCommitCommentsUrl :: Text
    , statusCommitHtmlUrl     :: Text
    , statusCommitAuthor      :: User
    , statusCommitCommitter   :: User
    , statusCommitParents     :: [CommitRefHtml]
    } deriving (Eq, Show, Read)


instance FromJSON StatusCommit where
    parseJSON (Object x) = StatusCommit
        <$> x .: "sha"
        <*> x .: "commit"
        <*> x .: "url"
        <*> x .: "comments_url"
        <*> x .: "html_url"
        <*> x .: "author"
        <*> x .: "committer"
        <*> x .: "parents"

    parseJSON _ = fail "StatusCommit"


instance ToJSON StatusCommit where
    toJSON StatusCommit{..} = object
        [ "sha"          .= statusCommitSha
        , "commit"       .= statusCommitCommit
        , "url"          .= statusCommitUrl
        , "comments_url" .= statusCommitCommentsUrl
        , "html_url"     .= statusCommitHtmlUrl
        , "author"       .= statusCommitAuthor
        , "committer"    .= statusCommitCommitter
        , "parents"      .= statusCommitParents
        ]
