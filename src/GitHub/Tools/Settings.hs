{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Tools.Settings
  ( syncSettings
  , validateSettings
  ) where

import           Control.Exception               (SomeException, try)
import           Control.Monad                   (forM_, unless, when)
import           Data.Aeson                      (FromJSON,
                                                  Value (Array, Null, Object, String),
                                                  encode, toJSON)
import qualified Data.Aeson.Key                  as AK
import qualified Data.Aeson.KeyMap               as KeyMap
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.List                       (isPrefixOf, nub, sortOn, (\\))
import           Data.Maybe                      (fromMaybe, isNothing,
                                                  mapMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Vector                     as V
import qualified GitHub
import           GitHub.Data.Request             (MediaType (..))
import qualified GitHub.Paths.Orgs               as Orgs
import qualified GitHub.Paths.Orgs.Teams         as Teams
import qualified GitHub.Paths.Orgs.Teams.Members as Members
import qualified GitHub.Paths.Repos              as Repos
import qualified GitHub.Paths.Repos.Branches     as Branches
import qualified GitHub.Paths.Repos.Labels       as Labels
import qualified GitHub.Paths.Repos.Rulesets     as Rulesets
import           GitHub.Tools.Requests           (mutate, mutate_, removeNulls,
                                                  request)
import           GitHub.Types.Base.User          (User (..))
import           GitHub.Types.Settings           (Label (Label, labelName),
                                                  OrgSettings (..),
                                                  RepoSettings (..),
                                                  Ruleset (..), Team (..),
                                                  TeamMembership (..))
import           Network.HTTP.Client             (Manager, newManager)
import           Network.HTTP.Client.TLS         (tlsManagerSettings)

delete :: Bool
delete = False

red :: String -> String
red s = "\ESC[31m" <> s <> "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" <> s <> "\ESC[0m"

-- Fields we can't meaningfully diff (read-only, not echoed, or a different shape).
ignoredKeys :: [String]
ignoredKeys = ["current_user_can_bypass", "automatic_copilot_code_review_enabled", "topics"]

diffV :: String -> Value -> Value -> [(String, Value, Value)]
diffV path (Object cur) (Object des) =
  concatMap step . filter (not . (`elem` ignoredKeys) . AK.toString . fst) $ KeyMap.toList des
  where
    step (k, dv) =
      let p  = if null path then AK.toString k else path <> "." <> AK.toString k
          cv = fromMaybe Null (KeyMap.lookup k cur)
      in diffV p cv dv
diffV path (Array cur) (Array des)
  | Just dm <- byType des, Just cm <- byType cur =
      concatMap (\(t, dv) -> diffV (at t) (fromMaybe Null (lookup t cm)) dv) dm
        ++ [ (at t, cv, Null) | (t, cv) <- cm, isNothing (lookup t dm) ]
  | length cur == length des =
      concat $ zipWith3 (\i c d -> diffV (path <> "[" <> show (i :: Int) <> "]") c d)
               [0 ..] (V.toList cur) (V.toList des)
  where
    at t = path <> "[" <> Text.unpack t <> "]"
    byType = mapM typed . V.toList
    typed (Object o) | Just (String t) <- KeyMap.lookup "type" o = Just (t, Object o)
    typed _ = Nothing
diffV path cur des
  | cur == des = []
  | otherwise  = [(path, cur, des)]

-- | The current value of something on GitHub, or Null if we can't get it.
fetchOrNull :: GitHub.Auth -> Manager -> GitHub.Request 'GitHub.RO Value -> IO Value
fetchOrNull auth mgr req = do
  res <- try (request (Just auth) mgr req)
  return $ either (const Null) id (res :: Either SomeException Value)

printDiff :: String -> [(String, Value, Value)] -> IO ()
printDiff header diffs =
  unless (null diffs) $ do
    putStrLn $ header <> ":"
    forM_ diffs $ \(p, cur, des) -> do
      putStrLn $ "  " <> p <> ":"
      putStrLn $ red   $ "    - " <> render cur
      putStrLn $ green $ "    + " <> render des
  where
    render Null = "(absent)"
    render v    = LBS.unpack (encode v)

report
  :: FromJSON a
  => Bool -> GitHub.Auth -> Manager -> String
  -> GitHub.Request 'GitHub.RW a -> IO ()
report dry auth mgr what req =
  if dry
    then putStrLn $ "~ would update " <> what
    else mutate auth mgr req >> return ()

report_
  :: Bool -> GitHub.Auth -> Manager -> String
  -> GitHub.GenRequest 'MtUnit 'GitHub.RW () -> IO ()
report_ dry auth mgr what req =
  if dry
    then putStrLn $ "~ would delete " <> what
    else mutate_ auth mgr req

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
  -> Bool
  -> IO ()
syncSettings auth org repos repoFilter dry = do
  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings
  let orgLogin = orgSettingsLogin org

  when (repoFilter `Text.isPrefixOf` orgLogin) $
    syncOrgSettings dry auth mgr org

  forM_ (sortOn fst . filterRepos . each $ repos) $ \(repo, repoSettings) -> do
    syncRepoSettings dry auth mgr orgLogin repo repoSettings
  where
    filterRepos = filter ((repoFilter `Text.isPrefixOf`) . fst)


syncOrgSettings :: Bool -> GitHub.Auth -> Manager -> OrgSettings -> IO ()
syncOrgSettings dry auth mgr OrgSettings{..} = do
  if dry
    then do
      current <- fetchOrNull auth mgr (Orgs.getOrgR orgSettingsLogin)
      printDiff ("org " <> Text.unpack orgSettingsLogin) (diffV "" current (removeNulls orgSettingsEditOrg))
    else report dry auth mgr ("org " <> Text.unpack orgSettingsLogin)
           (Orgs.editOrgR orgSettingsLogin orgSettingsEditOrg)
  syncOrgTeams dry auth mgr orgSettingsLogin orgSettingsTeams


syncOrgTeams :: Bool -> GitHub.Auth -> Manager -> Text -> HashMap Text Team -> IO ()
syncOrgTeams dry auth mgr orgLogin orgTeams = do
  teams <- request (Just auth) mgr (Teams.getTeamsR orgLogin)
  forM_ (HashMap.toList orgTeams) $ \(name, team) -> do
    let namedTeam = team
          { teamName = Just name
          , teamParentTeamId = teamParent team >>= teamName >>= getTeamId teams
          , teamMembers = Nothing  -- Synced later.
          }
    case getTeamId teams name of
      Just{} | dry -> do
        current <- fetchOrNull auth mgr (Teams.getTeamR orgLogin name)
        -- GitHub shows the parent, not parent_team_id, so the parent is compared by name.
        printDiff ("team " <> Text.unpack name)
          (diffV "" current (removeNulls . toJSON $ namedTeam{teamParentTeamId = Nothing}))
      Just{}  -> report dry auth mgr ("team " <> Text.unpack name)
                   (Teams.updateTeamR orgLogin name namedTeam)
      Nothing -> report dry auth mgr ("new team " <> Text.unpack name)
                   (Teams.createTeamR orgLogin namedTeam)
    forM_ (teamMembers team) $ syncTeamMembers dry auth mgr orgLogin name


syncTeamMembers :: Bool -> GitHub.Auth -> Manager -> Text -> Text -> HashMap Text Text -> IO ()
syncTeamMembers dry auth mgr orgLogin team teamMembers = do
  currentMembers <- request (Just auth) mgr (Members.getMembersR orgLogin team)
  -- Remove members that are not in the settings.
  forM_ (V.toList currentMembers) $ \User{userLogin} ->
    case HashMap.lookup userLogin teamMembers of
      Nothing ->
        putStrLn $ "Removing team member " <> Text.unpack userLogin <> " from " <> Text.unpack team
        -- mutate_ auth mgr (Members.deleteMemberR orgLogin team userLogin)
      Just{} -> return ()
  -- Add members that are in the settings but not in the team.
  forM_ (HashMap.toList teamMembers) $ \(login, role) ->
    case V.find ((login ==) . userLogin) currentMembers of
      Nothing ->
        report dry auth mgr ("add " <> Text.unpack role <> " " <> Text.unpack login <> " to team " <> Text.unpack team)
          (Members.addMemberR orgLogin team login (TeamMembership role))
      Just{} -> return ()
  -- Update roles of existing members if necessary.
  forM_ (HashMap.toList teamMembers) $ \(login, role) ->
    case V.find ((login ==) . userLogin) currentMembers of
      Nothing -> return ()
      Just{}  -> do
        currentMembership <- request (Just auth) mgr (Members.getMembershipR orgLogin team login)
        when (teamMembershipRole currentMembership /= role) $
          report dry auth mgr ("role of " <> Text.unpack login <> " in team " <> Text.unpack team <> " to " <> Text.unpack role)
            (Members.addMemberR orgLogin team login (TeamMembership role))


syncRepoSettings :: Bool -> GitHub.Auth -> Manager -> Text -> Text -> RepoSettings -> IO ()
syncRepoSettings dry auth mgr orgLogin repo RepoSettings{..} = do
  if dry
    then do
      current <- fetchOrNull auth mgr (Repos.getRepoR orgLogin repo)
      printDiff ("repo " <> Text.unpack repo) (diffV "" current (removeNulls repoSettingsEditRepo))
    else report dry auth mgr ("repo " <> Text.unpack repo)
           (Repos.editRepoR orgLogin repo repoSettingsEditRepo)
  syncLabels dry auth mgr orgLogin repo repoSettingsLabels
  forM_ (maybe [] each repoSettingsBranches) $ \(branch, update) ->
    if dry
      then diffBranchProtection auth mgr orgLogin repo branch update
      else mutate auth mgr (Branches.addProtectionR orgLogin repo branch update) >> return ()
  syncRepoRulesets dry auth mgr orgLogin repo repoSettingsRulesets


diffBranchProtection :: GitHub.Auth -> Manager -> Text -> Text -> Text -> Value -> IO ()
diffBranchProtection auth mgr orgLogin repo branch desired = do
  res <- try (request (Just auth) mgr (Branches.getProtectionR orgLogin repo branch))
  current <- case res :: Either SomeException Value of
    Left e  -> putStrLn ("  (could not fetch current protection: " <> show e <> ")") >> return Null
    Right v -> return v
  let cur     = contexts current
      des     = contexts desired
      removed = cur \\ des
      added   = des \\ cur
  unless (null removed && null added) $ do
    putStrLn $ "branch protection " <> Text.unpack repo <> "/" <> Text.unpack branch <> ":"
    forM_ removed $ \c -> putStrLn $ red   $ "  - " <> Text.unpack c
    forM_ added   $ \c -> putStrLn $ green $ "  + " <> Text.unpack c
  where
    contexts (Object o)
      | Just (Object r) <- KeyMap.lookup "required_status_checks" o
      = nub $ fromContexts r <> fromChecks r
    contexts _ = []
    fromContexts r = case KeyMap.lookup "contexts" r of
      Just (Array a) -> [t | String t <- V.toList a]
      _              -> []
    fromChecks r = case KeyMap.lookup "checks" r of
      Just (Array a) -> [t | Object c <- V.toList a, Just (String t) <- [KeyMap.lookup "context" c]]
      _              -> []


syncRepoRulesets :: Bool -> GitHub.Auth -> Manager -> Text -> Text -> Maybe (HashMap Text Ruleset) -> IO ()
syncRepoRulesets dry auth mgr orgLogin repo repoRulesets = do
  rulesets <- request (Just auth) mgr (Rulesets.getRulesetsR orgLogin repo)
  forM_ (maybe [] each repoRulesets) $ \(name, ruleset) -> do
    let namedRuleset = ruleset{rulesetName = Just name}
        hdr = "ruleset " <> Text.unpack repo <> "/" <> Text.unpack name
    case getRulesetId rulesets name of
      Just rId
        | dry -> do
            res <- try (request (Just auth) mgr (Rulesets.getRulesetR orgLogin repo rId))
            let current = either (const Null) id (res :: Either SomeException Value)
            printDiff hdr (diffV "" current (removeNulls namedRuleset))
        | otherwise -> mutate auth mgr (Rulesets.updateRulesetR orgLogin repo rId namedRuleset) >> return ()
      Nothing
        | dry       -> putStrLn $ green $ "+ would create " <> hdr
        | otherwise -> mutate auth mgr (Rulesets.addRulesetR orgLogin repo namedRuleset) >> return ()


syncLabels :: Bool -> GitHub.Auth -> Manager -> Text -> Text -> HashMap Text Label -> IO ()
syncLabels dry auth mgr orgLogin repo labels = do
  let newLabels = nub . map (\(name, label) -> (name, label{labelName = Just name})) . HashMap.toList $ labels
  oldLabels <- nub . map (\label@Label{labelName} -> (fromMaybe "" labelName, label)) . V.toList
    <$> request (Just auth) mgr (Labels.getLabelsR orgLogin repo)
  let changes = newLabels \\ oldLabels
      isUpdate lblName = any ((lblName ==) . fst) oldLabels
  if dry
    then unless (null changes) $ do
      putStrLn $ "labels " <> Text.unpack repo <> ":"
      forM_ changes $ \(lblName, _) ->
        putStrLn $ green $ "  " <> (if isUpdate lblName then "~ " else "+ ") <> Text.unpack lblName
    else do
      forM_ (oldLabels \\ newLabels) $ \(lblName, lbl) ->
        if delete
          then report_ dry auth mgr ("label " <> Text.unpack repo <> "/" <> Text.unpack lblName)
                 (Labels.deleteLabelR orgLogin repo lblName)
          else putStrLn $ "NOT deleting old label: " <> show lbl
      forM_ changes $ \(lblName, lbl) ->
        if isUpdate lblName
          then report dry auth mgr ("label " <> Text.unpack repo <> "/" <> Text.unpack lblName)
                 (Labels.updateLabelR orgLogin repo lblName lbl)
          else report dry auth mgr ("new label " <> Text.unpack repo <> "/" <> Text.unpack lblName)
                 (Labels.createLabelR orgLogin repo lbl)


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
