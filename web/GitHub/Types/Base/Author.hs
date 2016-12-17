{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.Base.Author where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Value (..), (.:), (.=))
import           Data.Text           (Text)

------------------------------------------------------------------------------
-- Author

data Author = Author
    { authorName     :: Text
    , authorEmail    :: Text
    , authorUsername :: Text
    } deriving (Eq, Show, Read)


instance FromJSON Author where
    parseJSON (Object x) = Author
        <$> x .: "name"
        <*> x .: "email"
        <*> x .: "username"

    parseJSON _ = fail "Author"


instance ToJSON Author where
    toJSON Author{..} = object
        [ "name"     .= authorName
        , "email"    .= authorEmail
        , "username" .= authorUsername
        ]
