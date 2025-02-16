{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Types.IssueTemplate where

import           Control.Applicative ((<|>))
import           Data.Aeson          (FromJSON (..), ToJSON (toJSON),
                                      Value (..))
import qualified Data.Aeson.Key      as Key
import qualified Data.Aeson.KeyMap   as KeyMap
import           Data.Aeson.TH       (Options (..), defaultOptions, deriveJSON)
import           Data.Aeson.Types    (parseEither)
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Vector         as V
import           GitHub.Types.Json   (removeNulls, valueIntersection)
import           Text.Casing         (kebab, quietSnake)

-- name: 🚀 Release
-- description: Build and deploy a new release
-- title: Release tracking issue
-- labels: [chore]
-- type: Task
-- body:
--   - type: textarea
--     id: release-notes
--     attributes:
--       label: Release notes
--       description: Write something nice about the new release.
--       placeholder: "Here's our latest awesome release!"
--     validations:
--       required: true
--   - type: dropdown
--     id: production
--     attributes:
--       label: Release type
--       description: Whether this is a production release or a release candidate.
--       options:
--         - Release candidate
--         - Production release
--     validations:
--       required: true

newtype BodyValidations = BodyValidations
    { bodyValidationsRequired :: Bool
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "BodyValidations")} ''BodyValidations)

data Body = Body
    { bodyType        :: Text
    , bodyId          :: Text
    , bodyAttributes  :: HashMap Text Value
    , bodyValidations :: BodyValidations
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "Body")} ''Body)

data IssueTemplate = IssueTemplate
    { issueTemplateName        :: Text
    , issueTemplateDescription :: Text
    , issueTemplateTitle       :: Text
    , issueTemplateLabels      :: [Text]
    , issueTemplateType        :: Text
    , issueTemplateBody        :: [Body]
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "IssueTemplate")} ''IssueTemplate)

parseIssueTemplate :: Value -> Either String IssueTemplate
parseIssueTemplate = parseEither parseJSON

issueTemplateIntersection :: IssueTemplate -> IssueTemplate -> IssueTemplate
issueTemplateIntersection a b =
    case parseIssueTemplate $ valueIntersection (removeNulls $ toJSON a) (removeNulls $ toJSON b) of
        Left  err -> error $ "issue template intersection is not parsable (should not happen): " <> err
        Right ok  -> ok
