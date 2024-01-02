{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Paths.Repos.Labels where

import           Data.Aeson            (encode)
import           Data.Text             (Text)
import           Data.Vector           (Vector)
import           GitHub.Data.Request   (CommandMethod (Delete, Patch, Post),
                                        FetchCount (FetchAll),
                                        GenRequest (Command), MediaType (..),
                                        RW (..), Request, command, pagedQuery)
import           GitHub.Types.Settings (Label)

getLabelsR :: Text -> Text -> Request 'RO (Vector Label)
getLabelsR user repo =
    pagedQuery ["repos", user, repo, "labels"] [] FetchAll

createLabelR :: Text -> Text -> Label -> Request 'RW Label
createLabelR user repo =
    command Post ["repos", user, repo, "labels"] . encode

updateLabelR :: Text -> Text -> Text -> Label -> Request 'RW Label
updateLabelR user repo name =
    command Patch ["repos", user, repo, "labels", name] . encode

deleteLabelR :: Text -> Text -> Text -> GenRequest 'MtUnit 'RW ()
deleteLabelR user repo name =
    Command Delete ["repos", user, repo, "labels", name] mempty
