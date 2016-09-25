module PullRequestInfo where

import qualified Data.Maybe            as Maybe
import qualified Data.Text             as Text
import qualified GitHub
import qualified Review
import           Text.Html             (prettyHtml, toHtml)
import           Text.Tabular          (Header (..), Properties (..),
                                        Table (..))
import qualified Text.Tabular.AsciiArt as AsciiArt
import qualified Text.Tabular.Html     as Html


data PullRequestInfo = PullRequestInfo
  { repoName     :: GitHub.Name GitHub.Repo
  , reviewStatus :: [Review.Status]
  , pullRequest  :: GitHub.PullRequest
  }


formatPR :: Bool -> [PullRequestInfo] -> String
formatPR False = AsciiArt.render id id id . prToTable
formatPR True  = prettyHtml . Html.render toHtml toHtml toHtml . prToTable


prToTable :: [PullRequestInfo] -> Table String String String
prToTable prs = Table rowNames columnNames rows
  where
    rowNames = Group NoLine $ map (Header . getRowName) prs
    getRowName pr =
      let repo num = getRepoName pr ++ " " ++ num in
      repo . show . GitHub.pullRequestNumber . pullRequest $ pr

    getRepoName = Text.unpack . GitHub.untagName . repoName

    columnNames =  Group SingleLine
      [ Header "branch"
      , Header "title"
      , Header "mergeable"
      , Header "mergeable_state"
      , Header "review_status"
      ]

    rows = map (\pr ->
      [ getPrBranch $ pullRequest pr
      , getPrTitle $ pullRequest pr
      , getPrMergeable $ pullRequest pr
      , getPrMergeableState $ pullRequest pr
      , show $ reviewStatus pr
      ]) prs

    getPrTitle = Text.unpack . GitHub.pullRequestTitle
    getPrMergeable = show . Maybe.fromMaybe False . GitHub.pullRequestMergeable
    getPrMergeableState = show . GitHub.pullRequestMergeableState

    getPrBranch =
      Text.unpack
        . GitHub.pullRequestCommitLabel
        . GitHub.pullRequestHead
