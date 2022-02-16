{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Reactions where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Reactions

data Reactions = Reactions
    { reactionsConfused   :: Int
    , reactionsEyes       :: Int
    , reactionsHeart      :: Int
    , reactionsHooray     :: Int
    , reactionsLaugh      :: Int
    , reactionsMinus1     :: Int
    , reactionsPlus1      :: Int
    , reactionsRocket     :: Int
    , reactionsTotalCount :: Int
    , reactionsUrl        :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Reactions where
    parseJSON (Object x) = Reactions
        <$> x .: "confused"
        <*> x .: "eyes"
        <*> x .: "heart"
        <*> x .: "hooray"
        <*> x .: "laugh"
        <*> x .: "-1"
        <*> x .: "+1"
        <*> x .: "rocket"
        <*> x .: "total_count"
        <*> x .: "url"

    parseJSON _ = fail "Reactions"


instance ToJSON Reactions where
    toJSON Reactions{..} = object
        [ "confused"    .= reactionsConfused
        , "eyes"        .= reactionsEyes
        , "heart"       .= reactionsHeart
        , "hooray"      .= reactionsHooray
        , "laugh"       .= reactionsLaugh
        , "-1"          .= reactionsMinus1
        , "+1"          .= reactionsPlus1
        , "rocket"      .= reactionsRocket
        , "total_count" .= reactionsTotalCount
        , "url"         .= reactionsUrl
        ]


instance Arbitrary Reactions where
    arbitrary = Reactions
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
