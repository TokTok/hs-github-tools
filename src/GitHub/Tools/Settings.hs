{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Tools.Settings
  ( syncSettings
  ) where

import           Control.Monad               (forM_)
import           Data.Aeson.TH               (Options (fieldLabelModifier),
                                              defaultOptions, deriveJSON)
import qualified Data.ByteString.Char8       as BS
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Yaml                   (Value, encode)
import qualified GitHub
import qualified GitHub.Paths.Repos          as Repos
import qualified GitHub.Paths.Repos.Branches as Branches
import           GitHub.Tools.Requests       (mutate)
import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Text.Casing                 (camel)

data Settings = Settings
  { settingsEditRepo :: Value
  , settingsBranches :: Maybe (HashMap Text Value)
  }
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Settings")} ''Settings)

syncSettings
  :: GitHub.Auth
  -> HashMap Text Settings
  -> Text
  -> IO ()
syncSettings auth repos repoFilter = do
  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings

  forM_ (filterRepos $ each repos) $ \(repo, Settings{..}) -> do
    editRes <- mutate auth mgr (Repos.editRepoR "TokTok" repo settingsEditRepo)
    BS.putStrLn $ encode editRes
    forM_ (maybe [] each settingsBranches) $ \(branch, update) -> do
      protRes <- mutate auth mgr (Branches.addProtectionR "TokTok" repo branch update)
      BS.putStrLn $ encode protRes
  where
    each = HashMap.toList . HashMap.filterWithKey (\k _ -> not $ "_" `Text.isPrefixOf` k)
    filterRepos = filter ((repoFilter `Text.isPrefixOf`) . fst)
