module PullRequestInfo where

import           Control.Monad         (join)
import qualified Data.Maybe            as Maybe
import qualified Data.Text             as Text
import           Data.Time.Clock       (UTCTime, diffUTCTime)
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


formatPR :: Bool -> UTCTime -> [[PullRequestInfo]] -> String
formatPR False now = AsciiArt.render id id id . prToTable now
formatPR True  now = prettyHtml . Html.render toHtml toHtml toHtml . prToTable now


prToTable :: UTCTime -> [[PullRequestInfo]] -> Table String String String
prToTable now prss = Table rowNames columnNames rows
  where
    rowNames = Group SingleLine
      . map (Group NoLine . map (Header . getRowName))
      . filter (not . null)
      $ prss

    getRowName pr =
      let repo num = getRepoName pr ++ " " ++ num in
      repo . show . GitHub.pullRequestNumber . pullRequest $ pr

    getRepoName = Text.unpack . GitHub.untagName . repoName

    columnNames =  Group SingleLine
      [ Header "branch"
      , Header "age"
      , Header "title"
      , Header "mergeable"
      , Header "mergeable_state"
      , Header "review_status"
      ]

    rows = map (\pr ->
      [ getPrBranch         $ pullRequest pr
      , getPrAge            $ pullRequest pr
      , getPrTitle          $ pullRequest pr
      , getPrMergeable      $ pullRequest pr
      , getPrMergeableState $ pullRequest pr
      , show $ reviewStatus pr
      ]) $ join prss

    getPrTitle = Text.unpack . GitHub.pullRequestTitle
    getPrMergeable = show . Maybe.fromMaybe False . GitHub.pullRequestMergeable
    getPrMergeableState = show . GitHub.pullRequestMergeableState

    getPrBranch =
      Text.unpack
        . GitHub.pullRequestCommitLabel
        . GitHub.pullRequestHead

    getPrAge =
      (++ "d") . show . diffInDays now . GitHub.pullRequestCreatedAt


diffInDays :: UTCTime -> UTCTime -> Int
diffInDays a b = round $ diffUTCTime a b / (60 * 60 * 24)
