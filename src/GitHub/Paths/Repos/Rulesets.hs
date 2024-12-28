{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Paths.Repos.Rulesets where

import           Data.Aeson            (Value, encode)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Vector           (Vector)
import           GitHub.Data.Request   (CommandMethod (Patch, Post, Put),
                                        FetchCount (FetchAll), RW (..), Request,
                                        command, pagedQuery)
import           GitHub.Types.Settings (Ruleset (Ruleset, rulesetId))

addRulesetR :: Text -> Text -> Ruleset -> Request 'RW Value
addRulesetR user repo =
    command Post ["repos", user, repo, "rulesets"] . encode

getRulesetsR :: Text -> Text -> Request 'RO (Vector Ruleset)
getRulesetsR user repo =
    pagedQuery ["repos", user, repo, "rulesets"] [] FetchAll

updateRulesetR :: Text -> Text -> Int -> Ruleset -> Request 'RW Value
updateRulesetR user repo rId =
    command Put ["repos", user, repo, "rulesets", Text.pack $ show rId] . encode
