{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.UserRef where

import           Control.Applicative       ((<$>), (<*>))
import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- UserRef

data UserRef = UserRef
    { userRefName  :: Text
    , userRefEmail :: Text
    } deriving (Eq, Show, Read)


instance FromJSON UserRef where
    parseJSON (Object x) = UserRef
        <$> x .: "name"
        <*> x .: "email"

    parseJSON _ = fail "UserRef"


instance ToJSON UserRef where
    toJSON UserRef{..} = object
        [ "name"  .= userRefName
        , "email" .= userRefEmail
        ]


instance Arbitrary UserRef where
    arbitrary = UserRef
        <$> arbitrary
        <*> arbitrary
