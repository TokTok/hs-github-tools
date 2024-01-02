{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Paths.Repo where

import           Data.Aeson                 (encode)
import           Data.Text                  (Text)
import           GitHub.Data.Request        (CommandMethod (Patch), RW (..),
                                             Request, command)
import           GitHub.Tools.Requests      (removeNulls)
import           GitHub.Types.Base.EditRepo (EditRepo)

editRepoR :: Text -> Text -> EditRepo -> Request 'RW EditRepo
editRepoR user repo =
    command Patch ["repos", user, repo] . encode . removeNulls
