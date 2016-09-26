{-# LANGUAGE OverloadedStrings #-}
module Review where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.List              as List
import qualified Data.Text              as Text
import qualified GitHub
import           Network.HTTP.Client    (Manager, httpLbs, parseRequest,
                                         responseBody)
import           Text.HTML.TagSoup.Tree (TagTree (..), parseTree)


data Approval
  = Approved
  | Rejected
  deriving (Show)

data Status = Status
  { reviewerName    :: BS.ByteString
  , _reviewerStatus :: Approval
  }

instance Show Status where
  show (Status name Approved) = '+' : read (show name)
  show (Status name Rejected) = '-' : read (show name)


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


extractApprovals :: [(BS.ByteString, BS.ByteString)] -> [Status]
extractApprovals = foldl extract []
  where
    extract acc (cls, name)
      | BS.isInfixOf "is-rejected" cls = Status name Rejected : acc
      | BS.isInfixOf "is-approved" cls = Status name Approved : acc
      | otherwise = acc


approvalsFromHtml :: BS.ByteString -> [Status]
approvalsFromHtml =
  nubWith reviewerName
  . extractApprovals
  . collectDiscussionItems
  . TagBranch "xml" []
  . parseTree
  where
    nubWith f = List.nubBy (\x y -> f x == f y)
