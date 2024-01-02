{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Tools.Settings
  ( syncSettings
  ) where

import           Control.Monad              (forM_)
import           Data.Aeson.TH              (Options (fieldLabelModifier),
                                             defaultOptions, deriveJSON)
import qualified Data.ByteString.Char8      as BS
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Yaml                  (encode)
import qualified GitHub
import qualified GitHub.Paths.Repo          as Repo
import           GitHub.Tools.Requests      (mutate)
import           GitHub.Types.Base.EditRepo (EditRepo)
import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Text.Casing                (camel)

newtype Settings = Settings
  { settingsEditRepo :: EditRepo
  }
$(deriveJSON defaultOptions{fieldLabelModifier = camel . drop (Text.length "Settings")} ''Settings)

syncSettings
  :: GitHub.Auth
  -> HashMap Text Settings
  -> IO ()
syncSettings auth repos = do
  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings

  forM_ (getRepos repos) $ \(repo, Settings{..}) -> do
    res <- mutate auth mgr (Repo.editRepoR "TokTok" repo settingsEditRepo)
    BS.putStrLn $ encode res
  where
    getRepos = HashMap.toList . HashMap.filterWithKey (\k _ -> not $ "_" `Text.isPrefixOf` k)
