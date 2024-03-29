{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Branch where

import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              object)
import           Data.Aeson.Types            (Value (..), (.:), (.=))
import           Data.Text                   (Text)
import           Test.QuickCheck.Arbitrary   (Arbitrary (..))

import           GitHub.Types.Base.CommitRef

------------------------------------------------------------------------------
-- Branch

data Branch = Branch
    { branchName      :: Text
    , branchProtected :: Bool
    , branchCommit    :: CommitRef
    } deriving (Eq, Show, Read)


instance FromJSON Branch where
    parseJSON (Object x) = Branch
        <$> x .: "name"
        <*> x .: "protected"
        <*> x .: "commit"

    parseJSON _ = fail "Branch"


instance ToJSON Branch where
    toJSON Branch{..} = object
        [ "name"      .= branchName
        , "protected" .= branchProtected
        , "commit"    .= branchCommit
        ]


instance Arbitrary Branch where
    arbitrary = Branch
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
