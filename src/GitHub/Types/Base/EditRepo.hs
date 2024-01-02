{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}
module GitHub.Types.Base.EditRepo where

import           Data.Aeson.TH                     (Options (fieldLabelModifier),
                                                    defaultOptions, deriveJSON)
import           GHC.Generics                      (Generic)
import           Test.QuickCheck.Arbitrary         (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import           Text.Casing                       (quietSnake)

------------------------------------------------------------------------------
-- EditRepo

data EditRepo = EditRepo
    { editRepoArchived                  :: Maybe Bool
    , editRepoHasProjects               :: Maybe Bool
    , editRepoDeleteBranchOnMerge       :: Maybe Bool
    , editRepoIsTemplate                :: Maybe Bool
    , editRepoAllowSquashMerge          :: Maybe Bool
    , editRepoHasWiki                   :: Maybe Bool
    , editRepoAllowForking              :: Maybe Bool
    , editRepoSquashMergeCommitMessage  :: Maybe String
    , editRepoAllowMergeCommit          :: Maybe Bool
    , editRepoWebCommitSignoffRequired  :: Maybe Bool
    , editRepoUseSquashPrTitleAsDefault :: Maybe Bool
    , editRepoDescription               :: Maybe String
    , editRepoSquashMergeCommitTitle    :: Maybe String
    , editRepoMergeCommitMessage        :: Maybe String
    , editRepoAllowUpdateBranch         :: Maybe Bool
    , editRepoPrivate                   :: Maybe Bool
    , editRepoName                      :: Maybe String
    , editRepoMergeCommitTitle          :: Maybe String
    , editRepoAllowRebaseMerge          :: Maybe Bool
    , editRepoHasIssues                 :: Maybe Bool
    , editRepoHomepage                  :: Maybe String
    , editRepoDefaultBranch             :: Maybe String
    , editRepoVisibility                :: Maybe String
    } deriving (Eq, Generic)
$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop (length "EditRepo")} ''EditRepo)

instance Arbitrary EditRepo where
    arbitrary = genericArbitrary
