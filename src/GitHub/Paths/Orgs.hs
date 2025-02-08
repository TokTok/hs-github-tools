{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Paths.Orgs where

import           Data.Aeson          (Value, encode)
import           Data.Text           (Text)
import           GitHub.Data.Request (CommandMethod (Patch), RW (..), Request,
                                      command)

editOrgR :: Text -> Value -> Request 'RW Value
editOrgR org =
    command Patch ["orgs", org] . encode
