{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Tools.AutoMerge (autoMergeAll) where

import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified GitHub
import           System.Posix.Directory       (changeWorkingDirectory)
import           System.Process               (callProcess)

import           GitHub.Tools.PullRequestInfo (PullRequestInfo (..))
import           GitHub.Tools.PullStatus      (getPullInfos)


trustedAuthors :: [Text]
trustedAuthors =
    [ "JFreegman"
    , "iphydf"
    , "nurupo"
    , "robinlinden"
    , "sudden6"
    , "zugz"
    ]


autoMerge
    :: String
    -> GitHub.Name GitHub.Organization
    -> PullRequestInfo
    -> IO ()
autoMerge _ _ PullRequestInfo{prOrigin = Nothing} = return ()
autoMerge token orgName PullRequestInfo{prRepoName, prUser, prBranch, prOrigin = Just prOrigin} = do
    let clonePath = "/tmp/automerge/" <> Text.unpack prRepoName
    callProcess "rm" ["-rf", clonePath]
    callProcess "git"
        [ "clone", "--depth=2"  -- 2 so we have a base commit (hopefully the master HEAD commit)
        , "--branch=" <> Text.unpack prBranch
        , "https://github.com/" <> Text.unpack prUser <> "/" <> Text.unpack prOrigin
        , clonePath
        ]
    changeWorkingDirectory clonePath

    callProcess "git"
        [ "remote", "add", "upstream"
        , "https://" <> token <> "@github.com/" <> Text.unpack (GitHub.untagName orgName) <> "/" <> Text.unpack prOrigin
        ]
    callProcess "git"
        [ "push", "upstream", Text.unpack prBranch <> ":master" ]


mergeable :: PullRequestInfo -> Bool
mergeable PullRequestInfo{prState, prTrustworthy, prUser} =
    prState == "clean" && (prTrustworthy || prUser `elem` trustedAuthors)


autoMergeAll
  :: GitHub.Name GitHub.Organization
  -> GitHub.Name GitHub.Owner
  -> String
  -> GitHub.Auth
  -> IO ()
autoMergeAll orgName ownerName token auth = do
    pulls <- filter mergeable . concat <$> getPullInfos orgName ownerName (Just auth)
    mapM_ (autoMerge token orgName) pulls
