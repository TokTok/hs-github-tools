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
import qualified GitHub.Paths.Repos.Rulesets as Rulesets
import           GitHub.Tools.Requests       (mutate, mutate_, request)
import           GitHub.Types.Settings       (Label (Label, labelName),
                                              RepoSettings (..), Ruleset (..))
import           Network.HTTP.Client         (Manager, newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)

debug :: Bool
debug = False

delete :: Bool
delete = False

getRulesetId :: V.Vector Ruleset -> Text -> Maybe Int
getRulesetId rulesets name = case V.find ((Just name ==) . rulesetName) rulesets of
  Just Ruleset{rulesetId = Just rId} -> return rId
  _                                  -> Nothing

syncSettings
  :: GitHub.Auth
  -> HashMap Text RepoSettings
  -> Text
  -> IO ()
syncSettings auth repos repoFilter = do
  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings

  forM_ (sortOn fst . filterRepos . each $ repos) $ \(repo, RepoSettings{..}) -> do
    editRes <- mutate auth mgr (Repos.editRepoR "TokTok" repo repoSettingsEditRepo)
    when debug $ BS.putStrLn $ encode editRes
    syncLabels auth mgr repo repoSettingsLabels
    forM_ (maybe [] each repoSettingsBranches) $ \(branch, update) -> do
      protRes <- mutate auth mgr (Branches.addProtectionR "TokTok" repo branch update)
      when debug $ BS.putStrLn $ encode protRes
    rulesets <- request (Just auth) mgr (Rulesets.getRulesetsR "TokTok" repo)
    forM_ (maybe [] each repoSettingsRulesets) $ \(name, ruleset) -> do
      let namedRuleset = ruleset{rulesetName = Just name}
      rulesetRes <- case getRulesetId rulesets name of
        Just rId -> mutate auth mgr (Rulesets.updateRulesetR "TokTok" repo rId namedRuleset)
        Nothing  -> mutate auth mgr (Rulesets.addRulesetR "TokTok" repo namedRuleset)
      when debug $ BS.putStrLn $ encode rulesetRes
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


validateSettings :: MonadFail m => HashMap Text RepoSettings -> m ()
validateSettings repos = do
  commonBranches <- case HashMap.lookup "_common" repos >>= repoSettingsBranches of
    Nothing -> fail "no _common section found"
    Just ok -> return ok
  commonContexts <- case HashMap.lookup "master" commonBranches of
    Nothing -> fail "no \"master\" branch in _common section found"
    Just ok -> getContexts "_common" "master" =<< getRequiredStatusChecks "_common" "master" ok
  -- Check that each repo's branch protection contexts start with the common ones.
  forM_ (filterRepos . each $ repos) $ \(repo, RepoSettings{..}) ->
    forM_ (maybe [] each repoSettingsBranches) $ \(branch, update) -> do
      contexts <- getContexts repo branch =<< getRequiredStatusChecks repo branch update
      let ctx = repo <> ".branches." <> branch <> ".required_status_checks.contexts"
      unless (commonContexts `isPrefixOf` contexts) $
        fail . Text.unpack $ ctx <> " should start with " <> Text.pack (show commonContexts)
      let dups = contexts \\ nub contexts
      unless (null dups) $
        fail . Text.unpack $ ctx <> " has duplicates: " <> Text.pack (show dups)

  where
    filterRepos = filter $ ("experimental" /=) . fst
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
