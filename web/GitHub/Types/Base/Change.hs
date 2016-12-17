{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.Change where

import           Control.Applicative ((<$>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

------------------------------------------------------------------------------
-- Change

data Change = Change
    { changesFrom :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Change where
    parseJSON (Object x) = Change
        <$> x .: "from"

    parseJSON _ = fail "Change"


instance ToJSON Change where
    toJSON Change{..} = object
        [ "from" .= changesFrom
        ]
