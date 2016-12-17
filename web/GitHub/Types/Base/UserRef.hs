{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.UserRef where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

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
