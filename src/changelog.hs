{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Arrow           (first, second, (&&&))
import qualified Data.ByteString.Char8   as BS8
import           Data.List               (foldl')
import qualified Data.Map                as Map
import qualified Data.Maybe              as Maybe
import           Data.Monoid             ((<>))
import           Data.String             (fromString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Vector             as V
import qualified GitHub
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Environment      (getArgs, getEnv)
-- import           Text.Groom              (groom)

import           Requests


type ChangeLog = [(Text, [Text], [Text])]


formatChangeLog :: Text -> ChangeLog -> Text
formatChangeLog name = (<> "\n") . foldl' (<>) ("# Changelog for " <> name) . map formatMilestone
  where
    formatMilestone (milestone, issues, pulls) =
      "\n\n## " <> milestone
      <> closedIssues issues
      <> mergedPrs pulls

    closedIssues [] = ""
    closedIssues issues =
      foldl' (<>) "\n\n### Closed issues:\n" . itemise $ issues

    mergedPrs [] = ""
    mergedPrs pulls =
      foldl' (<>) "\n\n### Merged PRs:\n" . itemise $ pulls

    itemise = map ("\n- " <>)


data ChangeLogItemKind
  = PullRequest
  | Issue
  deriving Show


data ChangeLogItem = ChangeLogItem
  { clMilestone :: Text
  , clTitle     :: Text
  , clNumber    :: Int
  }


formatChangeLogItem :: GitHub.Name GitHub.Owner -> GitHub.Name GitHub.Repo -> ChangeLogItem -> Text
formatChangeLogItem ownerName repoName item =
  "[#" <> number <> "](https://github.com/" <> GitHub.untagName ownerName <> "/" <> GitHub.untagName repoName <> "/issues/" <> number <> ") " <> clTitle item
  where
    number = Text.pack . show . clNumber $ item


makeChangeLog :: GitHub.Name GitHub.Owner -> GitHub.Name GitHub.Repo -> [GitHub.SimplePullRequest] -> [GitHub.Issue] -> ChangeLog
makeChangeLog ownerName repoName pulls issues =
  Map.foldlWithKey (\changes milestone (msIssues, msPulls) ->
      ( milestone
      , map (formatChangeLogItem ownerName repoName) msIssues
      , map (formatChangeLogItem ownerName repoName) msPulls
      ) : changes
    ) []
  . groupByMilestone (first  . (:)) changeLogIssues
  . groupByMilestone (second . (:)) changeLogPrs
  $ Map.empty
  where
    mergedPrs = filter (\case
        GitHub.SimplePullRequest
          { GitHub.simplePullRequestMergedAt = Just _
          } -> True
        _ -> False
      ) pulls

    closedItems = filter (\case
        GitHub.Issue
          { GitHub.issueMilestone = Just GitHub.Milestone
            { GitHub.milestoneState = "closed" }
          } -> True
        _ -> False
      ) issues

    milestoneByIssueId =
      Map.fromList
      . map (GitHub.issueNumber &&& GitHub.milestoneTitle . Maybe.fromJust . GitHub.issueMilestone)
      $ closedItems

    changeLogIssues :: [ChangeLogItem]
    changeLogIssues = Maybe.mapMaybe (\case
        -- filter out PRs
        GitHub.Issue { GitHub.issuePullRequest = Just _ } -> Nothing
        issue -> Just $ ChangeLogItem
          { clMilestone = GitHub.milestoneTitle . Maybe.fromJust . GitHub.issueMilestone $ issue
          , clTitle     = GitHub.issueTitle issue
          , clNumber    = GitHub.issueNumber issue
          }
      ) closedItems

    changeLogPrs :: [ChangeLogItem]
    changeLogPrs = Maybe.mapMaybe (\issue -> do
        milestone <- flip Map.lookup milestoneByIssueId . GitHub.simplePullRequestNumber $ issue
        return $ ChangeLogItem
          { clMilestone = milestone
          , clTitle     = GitHub.simplePullRequestTitle issue
          , clNumber    = GitHub.simplePullRequestNumber issue
          }
      ) mergedPrs

    groupByMilestone add = flip $ foldr (\item group ->
        Map.insert (clMilestone item) (
            case Map.lookup (clMilestone item) group of
              Just old -> add item old
              Nothing  -> add item ([], [])
          ) group
      )


fetchChangeLog :: GitHub.Name GitHub.Owner -> GitHub.Name GitHub.Repo -> IO ChangeLog
fetchChangeLog ownerName repoName = do
  -- Get auth token from the $GITHUB_TOKEN environment variable.
  token <- BS8.pack <$> getEnv "GITHUB_TOKEN"
  let auth = GitHub.OAuth token

  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings

  let pulls  = V.toList <$> request auth mgr (GitHub.pullRequestsForR ownerName repoName GitHub.stateClosed GitHub.FetchAll)
  let issues = V.toList <$> request auth mgr (GitHub.issuesForRepoR   ownerName repoName (GitHub.stateClosed <> GitHub.optionsAnyMilestone) GitHub.FetchAll)

  -- issues >>= putStrLn . groom

  makeChangeLog ownerName repoName <$> pulls <*> issues


main :: IO ()
main = do
  (ownerName, repoName) <- getArgs >>= repoLocation
  let name = (GitHub.untagName ownerName) <> "/" <> (GitHub.untagName repoName)

  fetchChangeLog ownerName repoName >>= putStr . Text.unpack . formatChangeLog name

  where
    repoLocation [] =
      return ("TokTok", "c-toxcore")
    repoLocation [ownerName, repoName] =
      return (fromString ownerName, fromString repoName)
    repoLocation _ =
      fail "Usage: changelog <owner> <repo>"
