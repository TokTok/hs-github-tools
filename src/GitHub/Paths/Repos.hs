{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Paths.Repos where

import           Data.Aeson          (Value, encode)
import           Data.Text           (Text)
import           GitHub.Data.Request (CommandMethod (Patch), RW (..), Request,
                                      command)

editRepoR :: Text -> Text -> Value -> Request 'RW Value
editRepoR user repo =
    command Patch ["repos", user, repo] . encode
