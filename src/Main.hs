{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad.Catch     (throwM)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.List               as List
import qualified Data.Maybe              as Maybe
import qualified Data.Text               as Text
import qualified Data.Vector             as V
import qualified GitHub
import qualified GitHub.Data.Id          as GitHub
import           Network.HTTP.Client     (Manager, httpLbs, newManager,
                                          parseRequest, responseBody)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Environment      (getEnv)
import           Text.HTML.TagSoup.Tree  (TagTree (..), parseTree)
import           Text.Tabular            (Header (..), Properties (..),
                                          Table (..))
import qualified Text.Tabular.AsciiArt   as Table


data Approval
  = Approved
  | Rejected
  deriving (Read, Show)

data PullRequestInfo = PullRequestInfo
  { reviewDone  :: String
  , pullRequest :: GitHub.PullRequest
  }


ownerName :: GitHub.Name GitHub.Owner
ownerName = "TokTok"

repoName :: GitHub.Name GitHub.Repo
repoName = "toxcore"


request :: GitHub.Auth -> Manager -> GitHub.Request k a -> IO a
request auth mgr req = do
  possiblePRs <- GitHub.executeRequestWithMgr mgr auth req
  case possiblePRs of
    Left  err -> throwM err
    Right res -> return res


fetchHtml :: Manager -> GitHub.SimplePullRequest -> IO BS.ByteString
fetchHtml mgr pr = do
  let url = Text.unpack $ GitHub.getUrl $ GitHub.simplePullRequestHtmlUrl pr
  putStrLn $ "fetching " ++ url
  req <- parseRequest url
  LBS.toStrict . responseBody <$> httpLbs req mgr


collectDiscussionItems :: TagTree BS.ByteString -> [(BS.ByteString, BS.ByteString)]
collectDiscussionItems = reverse . go []
  where
    go acc TagLeaf {} = acc

    go acc (TagBranch "div" [("class", cls)] (_ : TagBranch "div" _ (_ : TagBranch "a" [("href", name)] _ : _) : _))
      | BS.isInfixOf "discussion-item-review" cls = (cls, BS.tail name) : acc
    go acc (TagBranch _ _ body) =
      foldl go acc body


extractApprovals :: [(BS.ByteString, BS.ByteString)] -> [(BS.ByteString, Approval)]
extractApprovals = foldl extract []
  where
    extract acc (cls, name)
      | BS.isInfixOf "is-rejected" cls = (name, Rejected) : acc
      | BS.isInfixOf "is-approved" cls = (name, Approved) : acc
      | otherwise = acc


approvalsFromHtml :: BS.ByteString -> [(BS.ByteString, Approval)]
approvalsFromHtml =
  List.nubBy (\x y -> fst x == fst y)
  . extractApprovals
  . collectDiscussionItems
  . TagBranch "xml" []
  . parseTree


parseHtml :: BS.ByteString -> GitHub.PullRequest -> PullRequestInfo
parseHtml body pr = PullRequestInfo
  { reviewDone = show $ approvalsFromHtml body
  , pullRequest = pr
  }


main :: IO ()
main = do
  -- Get auth token from $HOME/.github-token.
  home <- getEnv "HOME"
  token <- BS.init <$> BS.readFile (home ++ "/.github-token")
  let auth = GitHub.OAuth token

  -- Initialise HTTP manager so we can benefit from keep-alive connections.
  mgr <- newManager tlsManagerSettings

  -- Get PR list.
  putStrLn "getting PR list from GitHub API"
  simplePRs <- V.toList <$> request auth mgr (GitHub.pullRequestsForR ownerName repoName GitHub.stateOpen GitHub.FetchAll)
  fullPrs <- mapM (request auth mgr . GitHub.pullRequestR ownerName repoName. GitHub.Id . GitHub.simplePullRequestNumber) simplePRs

  -- Fetch and parse HTML pages for each PR.
  prHtmls <- mapM (fetchHtml mgr) simplePRs
  let infos = zipWith parseHtml prHtmls fullPrs

  -- Pretty-print table with information.
  putStrLn $ formatPR infos


formatPR :: [PullRequestInfo] -> String
formatPR = Table.render id id id . prToTable


prToTable :: [PullRequestInfo] -> Table String String String
prToTable prs = Table rowNames columnNames rows
  where
    rowNames = Group NoLine $ map (Header . show . GitHub.pullRequestNumber . pullRequest) prs

    columnNames =  Group SingleLine
      [ Header "user"
      , Header "title"
      , Header "mergeable"
      , Header "mergeable_state"
      , Header "review_status"
      ]

    rows = map (\pr ->
      [ getPrAuthor $ pullRequest pr
      , show $ GitHub.pullRequestTitle $ pullRequest pr
      , show $ Maybe.fromMaybe False $ GitHub.pullRequestMergeable $ pullRequest pr
      , show $ GitHub.pullRequestMergeableState $ pullRequest pr
      , reviewDone pr
      ]) prs

    getPrAuthor =
      Text.unpack
      . GitHub.untagName
      . GitHub.simpleUserLogin
      . GitHub.pullRequestUser
