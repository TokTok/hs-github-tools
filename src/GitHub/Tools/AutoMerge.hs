{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Tools.AutoMerge
    ( autoMergePullRequest
    , autoMergeAll
    , trustedAuthors
    ) where

import qualified Data.ByteString.Char8        as BS8
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Vector                  as V
import qualified GitHub
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Posix.Directory       (changeWorkingDirectory)
import           System.Process               (callProcess)

import           GitHub.Tools.PullRequestInfo (PullRequestInfo (..))
import           GitHub.Tools.PullStatus      (getPrInfos, getPullInfos,
                                               makePullRequestInfo)
import           GitHub.Tools.Requests        (request)


trustedAuthors :: [Text]
trustedAuthors =
    [ "JFreegman"
    , "iphydf"
    , "nurupo"
    , "robinlinden"
    , "sudden6"
    , "zugz"
    ]


workDir :: FilePath
workDir = "/tmp/automerge"


autoMerge
    :: String
    -> GitHub.Name GitHub.Owner
    -> PullRequestInfo
    -> IO ()
autoMerge _ _ PullRequestInfo{prOrigin = Nothing} = return ()
autoMerge token ownerName PullRequestInfo{prRepoName, prUser, prBranch, prOrigin = Just prOrigin} = do
    let clonePath = workDir <> "/" <> Text.unpack prRepoName
    callProcess "rm" ["-rf", clonePath]
    callProcess "git"
        [ "clone", "--depth=6"  -- 6 so we can merge up to 5 commits on top of the master HEAD commit
        , "--branch=" <> Text.unpack prBranch
        , "https://github.com/" <> Text.unpack prUser <> "/" <> Text.unpack prOrigin
        , clonePath
        ]
    changeWorkingDirectory clonePath

    callProcess "git"
        [ "remote", "add", "upstream"
        , "https://" <> token <> "@github.com/" <> Text.unpack (GitHub.untagName ownerName) <> "/" <> Text.unpack prOrigin
        ]
    callProcess "git"
        [ "push", "upstream", Text.unpack prBranch <> ":master" ]

    -- Go back to a directory that will definitely exist even when next time
    -- we "rm -rf" the git repo cloned above.
    changeWorkingDirectory workDir


mergeable :: PullRequestInfo -> Bool
mergeable PullRequestInfo{prState, prTrustworthy, prUser} =
    prState == "clean" && (prTrustworthy || prUser `elem` trustedAuthors)


autoMergePullRequest
  :: String
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> IO ()
autoMergePullRequest token ownerName repoName = do
    let auth = Just . GitHub.OAuth . BS8.pack $ token
    mgr <- newManager tlsManagerSettings
    pulls <- (V.toList <$>
        request auth mgr (GitHub.pullRequestsForR ownerName repoName GitHub.stateOpen GitHub.FetchAll))
        >>= fmap (map $ makePullRequestInfo repoName) . getPrInfos auth mgr ownerName repoName
    putStrLn $ "found " <> show (length pulls) <> " pulls"

    let mergeablePulls = filter mergeable pulls
    putStrLn $ "selected " <> show (length mergeablePulls) <> " mergeable pulls:"
    mapM_ print mergeablePulls

    mapM_ (autoMerge token ownerName) mergeablePulls


autoMergeAll
  :: GitHub.Name GitHub.Organization
  -> GitHub.Name GitHub.Owner
  -> String
  -> IO ()
autoMergeAll orgName ownerName token = do
    let auth = Just . GitHub.OAuth . BS8.pack $ token
    pulls <- filter mergeable . concat <$> getPullInfos orgName ownerName auth
    mapM_ (autoMerge token ownerName) pulls
