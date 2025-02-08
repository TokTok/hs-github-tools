{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Paths.Orgs.Teams where

import           Data.Aeson            (Value, encode)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Vector           (Vector)
import           GitHub.Data.Request   (CommandMethod (Delete, Patch, Post),
                                        FetchCount (FetchAll), RW (..), Request,
                                        command, pagedQuery)
import           GitHub.Types.Settings (Team)

getTeamsR :: Text -> Request 'RO (Vector Team)
getTeamsR org =
    pagedQuery ["orgs", org, "teams"] [] FetchAll

createTeamR :: Text -> Team -> Request 'RW Value
createTeamR org =
    command Post ["orgs", org, "teams"] . encode

updateTeamR :: Text -> Text -> Team -> Request 'RW Value
updateTeamR org team =
    command Patch ["orgs", org, "teams", team] . encode

deleteTeamR :: Text -> Int -> Request 'RW Value
deleteTeamR org teamId =
    command Delete ["orgs", org, "teams", Text.pack $ show teamId] mempty
