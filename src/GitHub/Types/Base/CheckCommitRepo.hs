{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.CheckCommitRepo where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- CheckCommitRepo

data CheckCommitRepo = CheckCommitRepo
    { checkCommitRepoId   :: Int
    , checkCommitRepoName :: Text
    , checkCommitRepoUrl  :: Text
    } deriving (Eq, Show, Read)


instance FromJSON CheckCommitRepo where
    parseJSON (Object x) = CheckCommitRepo
        <$> x .: "id"
        <*> x .: "name"
        <*> x .: "url"

    parseJSON _ = fail "CheckCommitRepo"


instance ToJSON CheckCommitRepo where
    toJSON CheckCommitRepo{..} = object
        [ "id"           .= checkCommitRepoId
        , "name"         .= checkCommitRepoName
        , "url"          .= checkCommitRepoUrl
        ]


instance Arbitrary CheckCommitRepo where
    arbitrary = CheckCommitRepo
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
