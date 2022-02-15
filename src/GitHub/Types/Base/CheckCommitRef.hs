{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.CheckCommitRef where

import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    object)
import           Data.Aeson.Types                  (Value (..), (.:), (.=))
import           Data.Text                         (Text)
import           Test.QuickCheck.Arbitrary         (Arbitrary (..))

import           GitHub.Types.Base.CheckCommitRepo

------------------------------------------------------------------------------
-- CheckCommitRef

data CheckCommitRef = CheckCommitRef
    { checkCommitRefSha  :: Text
    , checkCommitRefRef  :: Text
    , checkCommitRefRepo :: CheckCommitRepo
    } deriving (Eq, Show, Read)


instance FromJSON CheckCommitRef where
    parseJSON (Object x) = CheckCommitRef
        <$> x .: "sha"
        <*> x .: "ref"
        <*> x .: "repo"

    parseJSON _ = fail "CheckCommitRef"


instance ToJSON CheckCommitRef where
    toJSON CheckCommitRef{..} = object
        [ "sha"          .= checkCommitRefSha
        , "ref"          .= checkCommitRefRef
        , "repo"         .= checkCommitRefRepo
        ]


instance Arbitrary CheckCommitRef where
    arbitrary = CheckCommitRef
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
