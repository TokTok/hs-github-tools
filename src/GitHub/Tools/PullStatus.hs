{-# LANGUAGE OverloadedStrings #-}
module GitHub.Tools.PullStatus
  ( getPullStatus
  , getPullInfos
  , showPullInfos
  ) where

import qualified Control.Monad.Parallel       as Parallel
import qualified Data.List                    as List
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Time.Clock              (getCurrentTime)
import qualified Data.Vector                  as V
import qualified GitHub
import           Network.HTTP.Client          (Manager, newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

import           GitHub.Tools.PullRequestInfo (PullRequestInfo (..))
import qualified GitHub.Tools.PullRequestInfo as PullRequestInfo
import           GitHub.Tools.Requests


getFullPr
  :: Maybe GitHub.Auth
  -> Manager
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> GitHub.SimplePullRequest
  -> IO GitHub.PullRequest
getFullPr auth mgr owner repo =
  request auth mgr
    . GitHub.pullRequestR owner repo
    . GitHub.simplePullRequestNumber


getPrInfo
  :: Maybe GitHub.Auth
  -> Manager
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> GitHub.SimplePullRequest
  -> IO ([Text], GitHub.PullRequest)
getPrInfo auth mgr ownerName repoName pr = do
  let assignees = V.toList $ GitHub.simplePullRequestAssignees pr
  let reviewers = map (GitHub.untagName . GitHub.simpleUserLogin) assignees
  -- Get more information that is only in the PullRequest response.
  fullPr <- getFullPr auth mgr ownerName repoName pr
  return (reviewers, fullPr)


makePullRequestInfo
  :: GitHub.Name GitHub.Repo
  -> ([Text], GitHub.PullRequest)
  -> PullRequestInfo
makePullRequestInfo repoName (reviewers, pr) = PullRequestInfo
  { prRepoName    = GitHub.untagName repoName
  , prNumber      = GitHub.unIssueNumber $ GitHub.pullRequestNumber pr
  , prUser        = user
  , prBranch      = Text.tail branch
  , prCreated     = GitHub.pullRequestCreatedAt pr
  , prTitle       = GitHub.pullRequestTitle pr
  , prReviewers   = reviewers
  , prState       = showMergeableState $ GitHub.pullRequestMergeableState pr
  , prOrigin      = GitHub.untagName . GitHub.repoName <$> GitHub.pullRequestCommitRepo (GitHub.pullRequestBase pr)
  -- TODO(iphydf): The Haskell github package doesn't support this yet.
  -- , prTrustworthy = GitHub.pullRequestAuthorAssociation pr
  , prTrustworthy = False
  }
  where
    (user, branch) = Text.breakOn ":" . GitHub.pullRequestCommitLabel . GitHub.pullRequestHead $ pr

    showMergeableState GitHub.StateBehind   = "behind"
    showMergeableState GitHub.StateBlocked  = "blocked"
    showMergeableState GitHub.StateClean    = "clean"
    showMergeableState GitHub.StateDirty    = "dirty"
    showMergeableState GitHub.StateDraft    = "draft"
    showMergeableState GitHub.StateUnknown  = "unknown"
    showMergeableState GitHub.StateUnstable = "unstable"


getPrsForRepo
  :: Maybe GitHub.Auth
  -> Manager
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> IO [PullRequestInfo]
getPrsForRepo auth mgr ownerName repoName =
  map (makePullRequestInfo repoName) <$> (
      -- Get PR list.
      V.toList <$> request auth mgr (GitHub.pullRequestsForR ownerName repoName GitHub.stateOpen GitHub.FetchAll)
      -- Get more details about each PR.
      >>= Parallel.mapM (getPrInfo auth mgr ownerName repoName))


getPullInfos
  :: GitHub.Name GitHub.Organization
  -> GitHub.Name GitHub.Owner
  -> Maybe GitHub.Auth
  -> IO [[PullRequestInfo]]
getPullInfos orgName ownerName auth = do
  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings

  -- Get repo list.
  repos <- V.toList <$> request auth mgr (GitHub.organizationReposR orgName GitHub.RepoPublicityAll GitHub.FetchAll)
  let repoNames = map GitHub.repoName repos

  filter (not . null) . List.sort <$> Parallel.mapM (getPrsForRepo auth mgr ownerName) repoNames


showPullInfos :: Bool -> [[PullRequestInfo]] -> IO Text
showPullInfos wantHtml infos =
  -- Pretty-print table with information.
  flip (PullRequestInfo.formatPR wantHtml) infos <$> getCurrentTime


getPullStatus
  :: GitHub.Name GitHub.Organization
  -> GitHub.Name GitHub.Owner
  -> Bool
  -> Maybe GitHub.Auth
  -> IO Text
getPullStatus orgName ownerName wantHtml auth =
  getPullInfos orgName ownerName auth >>= showPullInfos wantHtml
