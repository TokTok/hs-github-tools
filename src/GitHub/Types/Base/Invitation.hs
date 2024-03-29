{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Invitation where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Invitation

data Invitation = Invitation
    { invitationEmail :: Maybe Text
    , invitationId    :: Int
    , invitationLogin :: Text
    , invitationRole  :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Invitation where
    parseJSON (Object x) = Invitation
        <$> x .: "email"
        <*> x .: "id"
        <*> x .: "login"
        <*> x .: "role"

    parseJSON _ = fail "Invitation"


instance ToJSON Invitation where
    toJSON Invitation{..} = object
        [ "email"            .= invitationEmail
        , "id"               .= invitationId
        , "login"            .= invitationLogin
        , "role"             .= invitationRole
        ]


instance Arbitrary Invitation where
    arbitrary = Invitation
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
