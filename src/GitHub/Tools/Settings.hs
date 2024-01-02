{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Tools.Settings
  ( syncSettings
  , validateSettings
  ) where

import           Control.Monad               (forM_, unless, when)
import           Data.Aeson                  (Value (Array, Object, String))
import qualified Data.Aeson.KeyMap           as KeyMap
import qualified Data.ByteString.Char8       as BS
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.List                   (isPrefixOf, nub, sortOn, (\\))
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Vector                 as V
import           Data.Yaml                   (encode)
import qualified GitHub
import qualified GitHub.Paths.Repos          as Repos
import qualified GitHub.Paths.Repos.Branches as Branches
import qualified GitHub.Paths.Repos.Labels   as Labels
import           GitHub.Tools.Requests       (mutate, mutate_, request)
import           GitHub.Types.Settings       (Label (Label, labelName),
                                              Settings (..))
import           Network.HTTP.Client         (Manager, newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)

debug :: Bool
debug = False

delete :: Bool
delete = False

syncSettings
  :: GitHub.Auth
  -> HashMap Text Settings
  -> Text
  -> IO ()
syncSettings auth repos repoFilter = do
  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings

  forM_ (sortOn fst . filterRepos . each $ repos) $ \(repo, Settings{..}) -> do
    editRes <- mutate auth mgr (Repos.editRepoR "TokTok" repo settingsEditRepo)
    when debug $ BS.putStrLn $ encode editRes
    syncLabels auth mgr repo settingsLabels
    forM_ (maybe [] each settingsBranches) $ \(branch, update) -> do
      protRes <- mutate auth mgr (Branches.addProtectionR "TokTok" repo branch update)
      when debug $ BS.putStrLn $ encode protRes
  where
    filterRepos = filter ((repoFilter `Text.isPrefixOf`) . fst)


syncLabels :: GitHub.Auth -> Manager -> Text -> HashMap Text Label -> IO ()
syncLabels auth mgr repo labels = do
  putStrLn $ "Syncing labels to " <> Text.unpack repo
  let newLabels = nub . map (\(name, label) -> (name, label{labelName = Just name})) . HashMap.toList $ labels
  oldLabels <- nub . map (\label@Label{labelName} -> (fromMaybe "" labelName, label)) . V.toList
    <$> request (Just auth) mgr (Labels.getLabelsR "TokTok" repo)
  forM_ (oldLabels \\ newLabels) $ \(lblName, lbl) -> do
    if delete
      then do
        putStrLn $ "DELETING old label: " <> show lbl
        mutate_ auth mgr (Labels.deleteLabelR "TokTok" repo lblName)
      else putStrLn $ "NOT deleting old label: " <> show lbl
  forM_ (newLabels \\ oldLabels) $ \(lblName, lbl) -> do
    print lbl
    res <- if any ((lblName ==) . fst) oldLabels
      then mutate auth mgr (Labels.updateLabelR "TokTok" repo lblName lbl)
      else mutate auth mgr (Labels.createLabelR "TokTok" repo lbl)
    BS.putStrLn $ encode res


validateSettings :: MonadFail m => HashMap Text Settings -> m ()
validateSettings repos = do
  commonBranches <- case HashMap.lookup "_common" repos >>= settingsBranches of
    Nothing -> fail "no _common section found"
    Just ok -> return ok
  commonContexts <- case HashMap.lookup "master" commonBranches of
    Nothing -> fail "no \"master\" branch in _common section found"
    Just ok -> getContexts "_common" "master" =<< getRequiredStatusChecks "_common" "master" ok
  -- Check that each repo's branch protection contexts start with the common ones.
  forM_ (each repos) $ \(repo, Settings{..}) ->
    forM_ (maybe [] each settingsBranches) $ \(branch, update) -> do
      contexts <- getContexts repo branch =<< getRequiredStatusChecks repo branch update
      let ctx = repo <> ".branches." <> branch <> ".required_status_checks.contexts"
      unless (commonContexts `isPrefixOf` contexts) $
        fail . Text.unpack $ ctx <> " should start with " <> Text.pack (show contexts)
      let dups = contexts \\ nub contexts
      unless (null dups) $
        fail . Text.unpack $ ctx <> " has duplicates: " <> Text.pack (show dups)

  where
    getRequiredStatusChecks repo branch (Object mems) =
      case KeyMap.lookup "required_status_checks" mems of
        Nothing -> fail . Text.unpack $ repo <> ".branches." <> branch <> " should contain required_status_checks"
        Just ok -> return ok
    getRequiredStatusChecks repo branch _ =
      fail . Text.unpack $ repo <> ".branches." <> branch <> " should be an object"

    getContexts repo branch (Object mems) =
      case KeyMap.lookup "contexts" mems of
        Just (Array arr) -> return $ mapMaybe toString $ V.toList arr
        Just _ -> fail . Text.unpack $ repo <> ".branches." <> branch <> ".required_status_checks.contexts should be an array"
        Nothing -> fail . Text.unpack $ repo <> ".branches." <> branch <> ".required_status_checks should contain contexts"
    getContexts repo branch _ =
      fail . Text.unpack $ repo <> ".branches." <> branch <> ".required_status_checks should be an object"

    toString (String str) = Just str
    toString _            = Nothing

each :: HashMap Text a -> [(Text, a)]
each = HashMap.toList . HashMap.filterWithKey (\k _ -> not $ "_" `Text.isPrefixOf` k)
