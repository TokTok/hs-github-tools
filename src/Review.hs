{-# LANGUAGE OverloadedStrings #-}
module Review where

import qualified Data.ByteString.Lazy   as LBS
import qualified Data.List              as List
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8)
import qualified GitHub
import           Network.HTTP.Client    (Manager, httpLbs, parseRequest,
                                         responseBody)
import           Text.HTML.TagSoup.Tree (TagTree (..), parseTree)


data Approval
  = Approved
  | Rejected
  | Unknown
  deriving (Show)

data Status = Status
  { _reviewerStatus :: Approval
  , reviewerName    :: Text
  }

instance Show Status where
  show (Status Approved name) = '+' : read (show name)
  show (Status Rejected name) = '-' : read (show name)
  show (Status Unknown  name) =       read (show name)


fetchHtml :: Manager -> GitHub.SimplePullRequest -> IO Text
fetchHtml mgr pr = do
  let url = Text.unpack $ GitHub.getUrl $ GitHub.simplePullRequestHtmlUrl pr
  putStrLn $ "fetching " ++ url
  req <- parseRequest url
  decodeUtf8 . LBS.toStrict . responseBody <$> httpLbs req mgr


collectDiscussionItems :: TagTree Text -> [(Text, Text)]
collectDiscussionItems = reverse . go []
  where
    go acc TagLeaf {} = acc

    go acc (TagBranch "div" [("class", cls)] (_ : TagBranch "div" _ (_ : TagBranch "a" [("href", name)] _ : _) : _))
      | Text.isInfixOf "discussion-item-review" cls = (cls, Text.tail name) : acc
    go acc (TagBranch _ _ body) =
      foldl go acc body


extractApprovals :: [Status] -> [(Text, Text)] -> [Status]
extractApprovals = foldl extract
  where
    extract acc (cls, name)
      | Text.isInfixOf "is-rejected" cls = Status Rejected name : acc
      | Text.isInfixOf "is-approved" cls = Status Approved name : acc
      | otherwise = acc


approvalsFromHtml :: [Status] -> Text -> [Status]
approvalsFromHtml statuses =
  nubWith reviewerName
  . extractApprovals statuses
  . collectDiscussionItems
  . TagBranch "xml" []
  . parseTree
  where
    nubWith f = List.nubBy (\x y -> f x == f y)
