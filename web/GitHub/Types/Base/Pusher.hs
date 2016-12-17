{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.Pusher where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

------------------------------------------------------------------------------
-- Pusher

data Pusher = Pusher
    { pusherName  :: Text
    , pusherEmail :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Pusher where
    parseJSON (Object x) = Pusher
        <$> x .: "name"
        <*> x .: "email"

    parseJSON _ = fail "Pusher"


instance ToJSON Pusher where
    toJSON Pusher{..} = object
        [ "name"     .= pusherName
        , "email"    .= pusherEmail
        ]
