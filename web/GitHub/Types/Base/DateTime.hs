{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.DateTime where

import           Control.Applicative ((<$>), (<|>))
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Data.Text           (Text)

------------------------------------------------------------------------------
-- DateTime

data DateTime
    = DateTimeStamp Int
    | DateTimeText Text
    deriving (Eq, Show, Read)


instance FromJSON DateTime where
    parseJSON x =
          DateTimeStamp <$> parseJSON x
      <|> DateTimeText  <$> parseJSON x


instance ToJSON DateTime where
    toJSON (DateTimeStamp x) = toJSON x
    toJSON (DateTimeText  x) = toJSON x
