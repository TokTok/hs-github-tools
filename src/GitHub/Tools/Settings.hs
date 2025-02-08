{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Tools.Settings
  ( syncSettings
  , validateSettings
  ) where

import           Control.Monad                   (forM_, unless, when)
import           Data.Aeson                      (Value (Array, Object, String))
import qualified Data.Aeson.KeyMap               as KeyMap
import qualified Data.ByteString.Char8           as BS
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.List                       (isPrefixOf, nub, sortOn, (\\))
import           Data.Maybe                      (fromMaybe, mapMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Vector                     as V
import           Data.Yaml                       (encode)
import qualified GitHub
import qualified GitHub.Paths.Orgs               as Orgs
import qualified GitHub.Paths.Orgs.Teams         as Teams
import qualified GitHub.Paths.Orgs.Teams.Members as Members
import qualified GitHub.Paths.Repos              as Repos
import qualified GitHub.Paths.Repos.Branches     as Branches
import qualified GitHub.Paths.Repos.Labels       as Labels
import qualified GitHub.Paths.Repos.Rulesets     as Rulesets
import           GitHub.Tools.Requests           (mutate, mutate_, request)
import           GitHub.Types.Base.User          (User (..))
import           GitHub.Types.Settings           (Label (Label, labelName),
                                                  OrgSettings (..),
                                                  RepoSettings (..),
                                                  Ruleset (..), Team (..),
                                                  TeamMembership (..))
import           Network.HTTP.Client             (Manager, newManager)
import           Network.HTTP.Client.TLS         (tlsManagerSettings)

debug :: Bool
debug = False

delete :: Bool
delete = False

getRulesetId :: V.Vector Ruleset -> Text -> Maybe Int
getRulesetId rulesets name = case V.find ((Just name ==) . rulesetName) rulesets of
  Just Ruleset{rulesetId = Just rId} -> return rId
  _                                  -> Nothing

getTeamId :: V.Vector Team -> Text -> Maybe Int
getTeamId teams name = case V.find ((Just name ==) . teamName) teams of
  Just Team{teamId = Just tId} -> return tId
  _                            -> Nothing

syncSettings
  :: GitHub.Auth
  -> OrgSettings
  -> HashMap Text RepoSettings
  -> Text
  -> IO ()
syncSettings auth org repos repoFilter = do
  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings
  let orgLogin = orgSettingsLogin org

  when (repoFilter `Text.isPrefixOf` orgLogin) $
    syncOrgSettings auth mgr org

  forM_ (sortOn fst . filterRepos . each $ repos) $ \(repo, repoSettings) -> do
    syncRepoSettings auth mgr orgLogin repo repoSettings
  where
    filterRepos = filter ((repoFilter `Text.isPrefixOf`) . fst)


syncOrgSettings :: GitHub.Auth -> Manager -> OrgSettings -> IO ()
syncOrgSettings auth mgr OrgSettings{..} = do
  editRes <- mutate auth mgr (Orgs.editOrgR orgSettingsLogin orgSettingsEditOrg)
  when debug $ BS.putStrLn $ encode editRes
  syncOrgTeams auth mgr orgSettingsLogin orgSettingsTeams


syncOrgTeams :: GitHub.Auth -> Manager -> Text -> HashMap Text Team -> IO ()
syncOrgTeams auth mgr orgLogin orgTeams = do
  teams <- request (Just auth) mgr (Teams.getTeamsR orgLogin)
  forM_ (HashMap.toList orgTeams) $ \(name, team) -> do
    let namedTeam = team
          { teamName = Just name
          , teamParentTeamId = teamParent team >>= teamName >>= getTeamId teams
          , teamMembers = Nothing  -- Synced later.
          }
    teamRes <- case getTeamId teams name of
      Just{}  -> mutate auth mgr (Teams.updateTeamR orgLogin name namedTeam)
      Nothing -> do
        putStrLn $ "Creating team " <> Text.unpack name
        mutate auth mgr (Teams.createTeamR orgLogin namedTeam)
    when debug $ BS.putStrLn $ encode teamRes
    forM_ (teamMembers team) $ syncTeamMembers auth mgr orgLogin name


syncTeamMembers :: GitHub.Auth -> Manager -> Text -> Text -> HashMap Text Text -> IO ()
syncTeamMembers auth mgr orgLogin team teamMembers = do
  putStrLn $ "Syncing team members to " <> Text.unpack orgLogin <> "/" <> Text.unpack team
  currentMembers <- request (Just auth) mgr (Members.getMembersR orgLogin team)
  -- Remove members that are not in the settings.
  forM_ (V.toList currentMembers) $ \User{userLogin} ->
    case HashMap.lookup userLogin teamMembers of
      Nothing -> do
        putStrLn $ "Removing team member " <> Text.unpack userLogin <> " from " <> Text.unpack team
        -- mutate_ auth mgr (Members.deleteMemberR orgLogin team userLogin)
      Just{} -> return ()
  -- Add members that are in the settings but not in the team.
  forM_ (HashMap.toList teamMembers) $ \(login, role) ->
    case V.find ((login ==) . userLogin) currentMembers of
      Nothing -> do
        putStrLn $ "Adding team " <> Text.unpack role <> " " <> Text.unpack login <> " to " <> Text.unpack team
        res <- mutate auth mgr (Members.addMemberR orgLogin team login (TeamMembership role))
        when debug $ BS.putStrLn $ encode res
      Just{} -> return ()
  -- Update roles of existing members if necessary.
  forM_ (HashMap.toList teamMembers) $ \(login, role) -> do
    currentMembership <- request (Just auth) mgr (Members.getMembershipR orgLogin team login)
    when (teamMembershipRole currentMembership /= role) $ do
      putStrLn $ "Setting team member " <> Text.unpack login <> " role for team " <> Text.unpack team <> " to " <> Text.unpack role
      res <- mutate auth mgr (Members.addMemberR orgLogin team login (TeamMembership role))
      when debug $ BS.putStrLn $ encode res


syncRepoSettings :: GitHub.Auth -> Manager -> Text -> Text -> RepoSettings -> IO ()
syncRepoSettings auth mgr orgLogin repo RepoSettings{..} = do
  editRes <- mutate auth mgr (Repos.editRepoR orgLogin repo repoSettingsEditRepo)
  when debug $ BS.putStrLn $ encode editRes
  syncLabels auth mgr orgLogin repo repoSettingsLabels
  forM_ (maybe [] each repoSettingsBranches) $ \(branch, update) -> do
    protRes <- mutate auth mgr (Branches.addProtectionR orgLogin repo branch update)
    when debug $ BS.putStrLn $ encode protRes
  syncRepoRulesets auth mgr orgLogin repo repoSettingsRulesets


syncRepoRulesets :: GitHub.Auth -> Manager -> Text -> Text -> Maybe (HashMap Text Ruleset) -> IO ()
syncRepoRulesets auth mgr orgLogin repo repoRulesets = do
  rulesets <- request (Just auth) mgr (Rulesets.getRulesetsR orgLogin repo)
  forM_ (maybe [] each repoRulesets) $ \(name, ruleset) -> do
    let namedRuleset = ruleset{rulesetName = Just name}
    rulesetRes <- case getRulesetId rulesets name of
      Just rId -> mutate auth mgr (Rulesets.updateRulesetR orgLogin repo rId namedRuleset)
      Nothing  -> mutate auth mgr (Rulesets.addRulesetR orgLogin repo namedRuleset)
    when debug $ BS.putStrLn $ encode rulesetRes


syncLabels :: GitHub.Auth -> Manager -> Text -> Text -> HashMap Text Label -> IO ()
syncLabels auth mgr orgLogin repo labels = do
  putStrLn $ "Syncing labels to " <> Text.unpack repo
  let newLabels = nub . map (\(name, label) -> (name, label{labelName = Just name})) . HashMap.toList $ labels
  oldLabels <- nub . map (\label@Label{labelName} -> (fromMaybe "" labelName, label)) . V.toList
    <$> request (Just auth) mgr (Labels.getLabelsR orgLogin repo)
  forM_ (oldLabels \\ newLabels) $ \(lblName, lbl) -> do
    if delete
      then do
        putStrLn $ "DELETING old label: " <> show lbl
        mutate_ auth mgr (Labels.deleteLabelR orgLogin repo lblName)
      else putStrLn $ "NOT deleting old label: " <> show lbl
  forM_ (newLabels \\ oldLabels) $ \(lblName, lbl) -> do
    print lbl
    res <- if any ((lblName ==) . fst) oldLabels
      then mutate auth mgr (Labels.updateLabelR orgLogin repo lblName lbl)
      else mutate auth mgr (Labels.createLabelR orgLogin repo lbl)
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
