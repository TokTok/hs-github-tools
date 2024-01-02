{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Paths.Repos.Branches where

import           Data.Aeson          (Value, encode)
import           Data.Text           (Text)
import           GitHub.Data.Request (CommandMethod (Put), RW (..), Request,
                                      command)

addProtectionR :: Text -> Text -> Text -> Value -> Request 'RW Value
addProtectionR user repo branch =
    command Put ["repos", user, repo, "branches", branch, "protection"] . encode
