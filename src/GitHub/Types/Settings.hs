{-# LANGUAGE TemplateHaskell #-}
module GitHub.Types.Settings
  ( Label (..)
  , Settings (..)
  ) where

import           Data.Aeson          (Value)
import           Data.Aeson.TH       (Options (fieldLabelModifier),
                                      defaultOptions, deriveJSON)
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import           Text.Casing         (camel)

data Label = Label
  { labelName        :: Maybe Text
  , labelDescription :: Text
  , labelColor       :: Text
  } deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (length "Label")} ''Label)

data Settings = Settings
  { settingsEditRepo :: Value
  , settingsBranches :: Maybe (HashMap Text Value)
  , settingsLabels   :: HashMap Text Label
  } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (length "Settings")} ''Settings)
