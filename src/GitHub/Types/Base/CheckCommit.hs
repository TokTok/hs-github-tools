{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.CheckCommit where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

import           GitHub.Types.Base.Author

------------------------------------------------------------------------------
-- CheckCommit

data CheckCommit = CheckCommit
    { checkCommitAuthor    :: Author
    , checkCommitCommitter :: Author
    , checkCommitId        :: Text
    , checkCommitMessage   :: Text
    , checkCommitTimestamp :: Text
    , checkCommitTreeId    :: Text
    } deriving (Eq, Show, Read)


instance FromJSON CheckCommit where
    parseJSON (Object x) = CheckCommit
        <$> x .: "author"
        <*> x .: "committer"
        <*> x .: "id"
        <*> x .: "message"
        <*> x .: "timestamp"
        <*> x .: "tree_id"

    parseJSON _ = fail "CheckCommit"


instance ToJSON CheckCommit where
    toJSON CheckCommit{..} = object
        [ "author"       .= checkCommitAuthor
        , "committer"    .= checkCommitCommitter
        , "id"           .= checkCommitId
        , "message"      .= checkCommitMessage
        , "timestamp"    .= checkCommitTimestamp
        , "tree_id"      .= checkCommitTreeId
        ]


instance Arbitrary CheckCommit where
    arbitrary = CheckCommit
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
