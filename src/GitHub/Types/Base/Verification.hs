{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module GitHub.Types.Base.Verification where

import           Data.Aeson                (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types          (Value (..), (.:), (.=))
import           Data.Text                 (Text)
import           Data.Text.Arbitrary       ()
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

------------------------------------------------------------------------------
-- Verification

data Verification = Verification
    { verificationPayload   :: Maybe Text
    , verificationReason    :: Text
    , verificationSignature :: Maybe Text
    , verificationVerified  :: Bool
    } deriving (Eq, Show, Read)


instance FromJSON Verification where
    parseJSON (Object x) = Verification
        <$> x .: "payload"
        <*> x .: "reason"
        <*> x .: "signature"
        <*> x .: "verified"

    parseJSON _ = fail "Verification"


instance ToJSON Verification where
    toJSON Verification{..} = object
        [ "payload"   .= verificationPayload
        , "reason"    .= verificationReason
        , "signature" .= verificationSignature
        , "verified"  .= verificationVerified
        ]

instance Arbitrary Verification where
    arbitrary = Verification
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
