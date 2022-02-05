{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Changelogs
  ( fetchChangeLog
  , formatChangeLog
  , ChangeLog
  ) where

import           Control.Arrow           (first, second, (&&&))
import qualified Control.Monad.Parallel  as P
import           Data.List               (foldl')
import qualified Data.List               as List
import qualified Data.List.Split         as List
import qualified Data.Map                as Map
import qualified Data.Maybe              as Maybe
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Vector             as V
import           Debug.Trace             (traceIO)
import qualified GitHub
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Text.Read               (readMaybe)
-- import           Text.Groom              (groom)

import           Requests


newtype ChangeLog = ChangeLog { unChangeLog :: [(Text, [Text], [Text])] }


data VersionComponent
  = Number Int
  | Wildcard
  deriving (Ord, Eq)


instance Read VersionComponent where
  readsPrec _ ('x':s) = [(Wildcard, s)]
  readsPrec p input   = map (first Number) . readsPrec p $ input


data Version
  = VersionNumber [VersionComponent]
  | VersionString Text
  deriving (Ord, Eq)


formatChangeLog :: Bool -> ChangeLog -> Text
formatChangeLog wantRoadmap =
  (<> "\n") . foldl' (<>) "" . map formatMilestone . unChangeLog
  where
    prsWord    = if wantRoadmap then "PRs to review" else "Merged PRs"
    issuesWord = if wantRoadmap then "Planned tasks" else "Closed issues"

    formatMilestone (milestone, issues, pulls) =
      "\n\n## " <> milestone
      <> mergedPrs pulls
      <> closedIssues issues

    mergedPrs [] = ""
    mergedPrs pulls =
      foldl' (<>) ("\n\n### " <> prsWord <> ":\n") . itemise $ pulls

    closedIssues [] = ""
    closedIssues issues =
      foldl' (<>) ("\n\n### " <> issuesWord <> ":\n") . itemise $ issues

    itemise = map ("\n- " <>)


data ChangeLogItemKind
  = Issue
  | PullRequest
  deriving Show


data ChangeLogItem = ChangeLogItem
  { clMilestone :: Text
  , clTitle     :: Text
  , clNumber    :: Int
  }


groupByMilestone
  :: (ChangeLogItem -> ([a], [a]) -> ([a], [a]))
  -> [ChangeLogItem]
  -> Map.Map Text ([a], [a])
  -> Map.Map Text ([a], [a])
groupByMilestone add = flip $ foldr (\item group ->
    Map.insert (clMilestone item) (
        case Map.lookup (clMilestone item) group of
          Just old -> add item old
          Nothing  -> add item ([], [])
      ) group)


formatChangeLogItem
  :: ChangeLogItemKind
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> ChangeLogItem
  -> Text
formatChangeLogItem kind ownerName repoName item =
  "[#" <> number <> "](https://github.com/" <> GitHub.untagName ownerName <>
  "/" <> GitHub.untagName repoName <> kindPart <> number <> ") " <>
  clTitle item
  where
    number = Text.pack . show . clNumber $ item
    kindPart =
      case kind of
        Issue       -> "/issues/"
        PullRequest -> "/pull/"


makeChangeLog
  :: Bool
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> [GitHub.SimplePullRequest]
  -> [GitHub.Issue]
  -> ChangeLog
makeChangeLog wantRoadmap ownerName repoName pulls issues =
  ChangeLog
  . reverseIfChangelog
  . sortChangelog
  . Map.foldlWithKey (\changes milestone (msIssues, msPulls) ->
      if milestone == "meta"
        then changes
        else
          ( milestone
          , map (formatChangeLogItem Issue       ownerName repoName) msIssues
          , map (formatChangeLogItem PullRequest ownerName repoName) msPulls
          ) : changes
    ) []
  . groupByMilestone (first  . (:)) changeLogIssues
  . groupByMilestone (second . (:)) changeLogPrs
  $ Map.empty
  where
    reverseIfChangelog l =
      if wantRoadmap
        then l
        else case reverse l of
          []   -> []
          x:xs -> xs ++ [x] -- Put "Backlog" last.

    sortChangelog :: [(Text, [Text], [Text])] -> [(Text, [Text], [Text])]
    sortChangelog = List.sortOn $ \(v, _, _) -> parseVersion v

    parseVersion :: Text -> Version
    parseVersion v =
        case Text.unpack v of
            -- Milestones starting with "v" must be versions.
            'v':version ->
                case map readMaybe $ List.splitOn "." version of
                  [Just major, Just minor, Just rev] -> VersionNumber [major, minor, rev]
                  _ -> error $ "invalid version: " <> version
            _ -> VersionString v

    (mergedPrs, openPrs) = List.partition (\case
        GitHub.SimplePullRequest
          { GitHub.simplePullRequestMergedAt = Just _ } -> True
        _                                               -> False
      ) pulls

    selectedPrs =
      if wantRoadmap
        then openPrs
        else mergedPrs

    selectedItems = filter (\case
        GitHub.Issue
          { GitHub.issueMilestone = Just GitHub.Milestone
            { GitHub.milestoneState = state }
          } ->
            if wantRoadmap
              then state == "open"
              else state == "closed"
        _ -> False
      ) issues

    (backlogPrs, backlogIssues) =
      if wantRoadmap
        then
            -- Split into PRs and non-PR issues.
            List.partition (\case
              GitHub.Issue { GitHub.issuePullRequest = Just _ } -> True
              _                                                 -> False
            )
            -- Filter by issues that don't have a milestone.
          . filter (\case
              GitHub.Issue { GitHub.issueMilestone = Nothing } -> True
              _                                                -> False
            )
          $ issues
        else
          ([], [])

    addToBacklog =
      map $ \issue -> ChangeLogItem
        { clMilestone = "Backlog"
        , clTitle     = GitHub.issueTitle issue
        , clNumber    = GitHub.unIssueNumber $ GitHub.issueNumber issue
        }

    backlogPrItems    = addToBacklog backlogPrs
    backlogIssueItems = addToBacklog backlogIssues

    milestoneByIssueId =
      Map.fromList
      . map (GitHub.issueNumber &&& GitHub.milestoneTitle . Maybe.fromJust . GitHub.issueMilestone)
      $ selectedItems

    changeLogIssues :: [ChangeLogItem]
    changeLogIssues = backlogIssueItems ++ Maybe.mapMaybe (\case
        -- filter out PRs
        GitHub.Issue { GitHub.issuePullRequest = Just _ } -> Nothing
        issue -> Just ChangeLogItem
          { clMilestone = GitHub.milestoneTitle . Maybe.fromJust . GitHub.issueMilestone $ issue
          , clTitle     = GitHub.issueTitle issue
          , clNumber    = GitHub.unIssueNumber $ GitHub.issueNumber issue
          }
      ) selectedItems

    changeLogPrs :: [ChangeLogItem]
    changeLogPrs = backlogPrItems ++ Maybe.mapMaybe (\issue -> do
        milestone <- flip Map.lookup milestoneByIssueId . GitHub.simplePullRequestNumber $ issue
        return ChangeLogItem
          { clMilestone = milestone
          , clTitle     = GitHub.simplePullRequestTitle issue
          , clNumber    = GitHub.unIssueNumber $ GitHub.simplePullRequestNumber issue
          }
      ) selectedPrs


fetchChangeLog
  :: Bool
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> Maybe GitHub.Auth
  -> IO ChangeLog
fetchChangeLog wantRoadmap ownerName repoName auth = do
  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings

  let fetchPulls state = do
          traceIO $ "Fetching pull requests"
          l <- V.toList <$> request auth mgr (GitHub.pullRequestsForR ownerName repoName state GitHub.FetchAll)
          traceIO $ "Got " <> show (length l) <> " pull requests"
          return l
  let fetchIssues state = do
          traceIO $ "Fetching issues"
          l <- V.toList <$> request auth mgr (GitHub.issuesForRepoR   ownerName repoName state GitHub.FetchAll)
          traceIO $ "Got " <> show (length l) <> " issues"
          return l

  -- issues >>= putStrLn . groom

  (pulls, issues) <- uncurry (P.bindM2 (\p i -> return (p, i))) $
          if wantRoadmap
             then (fetchPulls GitHub.stateOpen  , fetchIssues GitHub.stateOpen  )
             else (fetchPulls GitHub.stateClosed, fetchIssues GitHub.stateClosed)

  traceIO "Formatting changelog/roadmap"
  return $ makeChangeLog wantRoadmap ownerName repoName pulls issues
