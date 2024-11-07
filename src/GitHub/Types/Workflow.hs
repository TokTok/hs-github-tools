{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Types.Workflow where

import           Control.Applicative ((<|>))
import           Data.Aeson          (FromJSON (..), ToJSON (toJSON),
                                      Value (..))
import qualified Data.Aeson.Key      as Key
import           Data.Aeson.KeyMap   (KeyMap)
import qualified Data.Aeson.KeyMap   as KeyMap
import           Data.Aeson.TH       (Options (..), defaultOptions, deriveJSON)
import           Data.Aeson.Types    (parseEither)
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Vector         as V
import           Debug.Trace         (trace)
import           Text.Casing         (kebab, quietSnake)

data Input = Input
    { inputDefault  :: Maybe Text
    , inputRequired :: Bool
    , inputType     :: Text
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "Input")} ''Input)

newtype Secret = Secret
    { secretRequired :: Bool
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "Secret")} ''Secret)

data OnSpec = OnSpec
    { onSpecBranches :: Maybe [Text]
    , onSpecInputs   :: Maybe (HashMap Text Input)
    , onSpecSecrets  :: Maybe (HashMap Text Secret)
    , onSpecTypes    :: Maybe [Text]
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "OnSpec")} ''OnSpec)

newtype OnSchedule = OnSchedule
    { onScheduleCron :: Text
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "OnSchedule")} ''OnSchedule)

data OnMap = OnMap
    { onMapPullRequest       :: Maybe OnSpec
    , onMapPullRequestTarget :: Maybe OnSpec
    , onMapPush              :: Maybe OnSpec
    , onMapRelease           :: Maybe OnSpec
    , onMapSchedule          :: Maybe [OnSchedule]
    , onMapWorkflowCall      :: Maybe OnSpec
    , onMapWorkflowDispatch  :: Maybe OnSpec
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop (Text.length "OnMap")} ''OnMap)

data OneOf a b
    = A a
    | B b
    deriving (Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (OneOf a b) where
    toJSON (A x) = toJSON x
    toJSON (B x) = toJSON x

instance (FromJSON a, FromJSON b) => FromJSON (OneOf a b) where
    parseJSON x = A <$> parseJSON x <|> B <$> parseJSON x

data Step = Step
    { stepId   :: Maybe Text
    , stepIf   :: Maybe Text
    , stepName :: Maybe Text
    , stepEnv  :: Maybe (HashMap Text Text)
    , stepRun  :: Maybe Text
    , stepWith :: Maybe (HashMap Text Value)
    , stepUses :: Maybe Text
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "Step")} ''Step)

newtype RunConfig = RunConfig
    { runConfigShell :: Maybe Text
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "RunConfig")} ''RunConfig)

newtype JobDefaults = JobDefaults
    { jobDefaultsRun  :: Maybe RunConfig
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "JobDefaults")} ''JobDefaults)

data Permission
    = PermissionRead
    | PermissionWrite
    deriving (Show, Eq)
$(deriveJSON defaultOptions{constructorTagModifier = kebab . drop (Text.length "Permission")} ''Permission)

data PermissionsMap = PermissionsMap
    { permissionsMapContents     :: Maybe Permission
    , permissionsMapPullRequests :: Maybe Permission
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "PermissionsMap")} ''PermissionsMap)

data PermissionsString
    = PermissionsStringReadAll
    | PermissionsStringWriteAll
    deriving (Show, Eq)
$(deriveJSON defaultOptions{constructorTagModifier = kebab . drop (Text.length "PermissionsString")} ''PermissionsString)

type Permissions = OneOf PermissionsMap PermissionsString

data Strategy = Strategy
    { strategyFailFast :: Maybe Bool
    , strategyMatrix   :: HashMap Text [Value]
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "Strategy")} ''Strategy)

data Job = Job
    { jobDefaults    :: Maybe JobDefaults
    , jobEnv         :: Maybe (HashMap Text Text)
    , jobContainer   :: Maybe Text
    , jobName        :: Maybe Text
    , jobNeeds       :: Maybe [Text]
    , jobPermissions :: Maybe Permissions
    , jobRunsOn      :: Maybe Text
    , jobSecrets     :: Maybe (HashMap Text Text)
    , jobSteps       :: Maybe [Step]
    , jobStrategy    :: Maybe Strategy
    , jobUses        :: Maybe Text
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "Job")} ''Job)

data Concurrency = Concurrency
    { concurrencyGroup            :: Text
    , concurrencyCancelInProgress :: Bool
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "Concurrency")} ''Concurrency)

type On = OneOf OnMap [Text]

data Spec = Spec
    { specConcurrency :: Maybe Concurrency
    , specEnv         :: Maybe (HashMap Text Text)
    , specName        :: Maybe Text
    , specOn          :: On
    , specPermissions :: Maybe Permissions
    , specJobs        :: HashMap Text Job
    }
    deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = kebab . drop (Text.length "Spec")} ''Spec)

parseSpec :: Value -> Either String Spec
parseSpec = parseEither parseJSON

removeNulls :: ToJSON a => a -> Value
removeNulls = go . toJSON
  where
    go (Array  x) = Array . V.map go $ x
    go (Object x) = Object . KeyMap.map go . KeyMap.filterWithKey validPair $ x
    go         x  = x

    isEmpty Null      = True
    isEmpty (Array x) = null x
    isEmpty _         = False

    validPair k v = not (isEmpty v || "x-" `Text.isPrefixOf` Key.toText k)

valueIntersection :: Value -> Value -> Value
valueIntersection (Object x) (Object y) = Object $ KeyMap.intersectionWith valueIntersection x y
valueIntersection (Array  x) (Array  y) = Array  $ V.filter (/= Null) $ V.zipWith valueIntersection x y
valueIntersection _ y = y

specIntersection :: Spec -> Spec -> Spec
specIntersection a b =
    case parseSpec $ valueIntersection (removeNulls $ toJSON a) (removeNulls $ toJSON b) of
        Left  err -> error $ "workflow spec intersection is not parseable (should not happen): " <> err
        Right ok  -> ok
