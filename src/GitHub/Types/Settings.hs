{-# LANGUAGE TemplateHaskell #-}
module GitHub.Types.Settings
  ( Label (..)
  , Ruleset (..)
  , RepoSettings (..)
  ) where

import           Data.Aeson          (Value)
import           Data.Aeson.TH       (Options (fieldLabelModifier),
                                      defaultOptions, deriveJSON)
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import           Text.Casing         (camel, quietSnake)

data Label = Label
  { labelName        :: Maybe Text
  , labelDescription :: Maybe Text
  , labelColor       :: Text
  } deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop (length "Label")} ''Label)

data Ruleset = Ruleset
  { rulesetId                   :: Maybe Int
  , rulesetName                 :: Maybe Text
  , rulesetTarget               :: Text
  , rulesetSourceType           :: Maybe Text
  , rulesetSource               :: Maybe Text
  , rulesetEnforcement          :: Text
  , rulesetNodeId               :: Maybe Text
  , rulesetConditions           :: Maybe (HashMap Text Value)
  , rulesetRules                :: Maybe [HashMap Text Value]
  , rulesetCreatedAt            :: Maybe Text
  , rulesetUpdatedAt            :: Maybe Text
  , rulesetBypassActors         :: Maybe [HashMap Text Value]
  , rulesetCurrentUserCanBypass :: Maybe Text
  , rulesetLinks                :: Maybe (HashMap Text Value)
  } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop (length "Ruleset")} ''Ruleset)

data RepoSettings = RepoSettings
  { repoSettingsEditRepo :: Value
  , repoSettingsBranches :: Maybe (HashMap Text Value)
  , repoSettingsRulesets :: Maybe (HashMap Text Ruleset)
  , repoSettingsLabels   :: HashMap Text Label
  } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (length "RepoSettings")} ''RepoSettings)
