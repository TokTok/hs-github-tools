{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Paths.Orgs.Teams.Members where

import           Data.Aeson             (encode)
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import           GitHub.Data.Request    (CommandMethod (Delete, Put),
                                         FetchCount (FetchAll),
                                         GenRequest (Command), MediaType (..),
                                         RW (..), Request, command, pagedQuery,
                                         query)
import           GitHub.Types.Base.User (User)
import           GitHub.Types.Settings  (TeamMembership)

getMembersR :: Text -> Text -> Request 'RO (Vector User)
getMembersR org team =
    pagedQuery ["orgs", org, "teams", team, "members"] [] FetchAll

addMemberR :: Text -> Text -> Text -> TeamMembership -> Request 'RW TeamMembership
addMemberR org team member =
    command Put ["orgs", org, "teams", team, "memberships", member] . encode

getMembershipR :: Text -> Text -> Text -> Request 'RO TeamMembership
getMembershipR org team member =
    query ["orgs", org, "teams", team, "memberships", member] []

deleteMemberR :: Text -> Text -> Text -> GenRequest 'MtUnit 'RW ()
deleteMemberR org team member =
    Command Delete ["orgs", org, "teams", team, "members", member] mempty
