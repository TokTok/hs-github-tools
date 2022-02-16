{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module TokTok.Hello (newApp) where

import           Caching.ExpiringCacheMap.HashECM (CacheSettings (..), ECM,
                                                   consistentDuration,
                                                   lookupECM, newECMIO)
import           Control.Concurrent.MVar          (MVar)
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8            as BS8
import qualified Data.ByteString.Lazy             as LBS
import           Data.HashMap.Strict              (HashMap)
import           Data.Text                        (Text)
import           Data.Text.Encoding               (encodeUtf8)
import qualified Data.Time.Clock.POSIX            as POSIX
import qualified GitHub
import           Network.HTTP.Media               ((//), (/:))
import           Servant
import           System.Environment               (getEnv)

import qualified GitHub.Tools.Changelogs          as Changelogs
import           GitHub.Tools.PullRequestInfo     (PullRequestInfo)
import qualified GitHub.Tools.PullStatus          as PullStatus
import qualified GitHub.Tools.NetworkGraph          as NetworkGraph


type GitHubCache a = ECM IO MVar () HashMap () a


data ApiContext = ApiContext
    { changelogInfo :: GitHubCache Changelogs.ChangeLog
    , roadmapInfo   :: GitHubCache Changelogs.ChangeLog
    , pullInfos     :: GitHubCache [[PullRequestInfo]]
    , networkGraph  :: GitHubCache String
    }


newGitHubCache :: Int -> IO a -> IO (GitHubCache a)
newGitHubCache timeout fetchUpdates = newECMIO
    (consistentDuration timeout $ \state () -> do
        infos <- fetchUpdates
        return (state, infos)
    )
    (round <$> POSIX.getPOSIXTime)
    1
    (CacheWithLRUList 1 1 1)


newContext :: IO ApiContext
newContext = do
    auth <- Just . GitHub.OAuth . BS8.pack <$> getEnv "GITHUB_TOKEN"

    ApiContext
        <$> newGitHubCache 300 (Changelogs.fetchChangeLog False "TokTok" "c-toxcore" auth)
        <*> newGitHubCache 300 (Changelogs.fetchChangeLog True  "TokTok" "c-toxcore" auth)
        <*> newGitHubCache  30 (PullStatus.getPullInfos "TokTok" "TokTok" auth)
        <*> newGitHubCache 900 (NetworkGraph.getNetworkGraph auth [("TokTok", "c-toxcore"), ("irungentoo", "toxcore")])


data HTML
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML Text where
    mimeRender _ = LBS.fromStrict . encodeUtf8

-- API specification
type TestApi =
       -- Link to the source code repository, to comply with AGPL.
       "source" :> Get '[PlainText] Text
  :<|> "changelog" :> Get '[PlainText] Text
  :<|> "roadmap" :> Get '[PlainText] Text
  :<|> "pulls.html" :> Get '[HTML] Text
  :<|> "pulls" :> Get '[JSON] [[PullRequestInfo]]
  :<|> "network" :> Get '[PlainText] String

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: ApiContext -> Server TestApi
server ctx =
       sourceH
  :<|> changelogH
  :<|> roadmapH
  :<|> pullsHtmlH
  :<|> pullsH
  :<|> networkH
  where
    sourceH = return "https://github.com/TokTok/hs-github-tools"

    changelogH = liftIO $
        Changelogs.formatChangeLog False <$> lookupECM (changelogInfo ctx) ()
    roadmapH = liftIO $
        Changelogs.formatChangeLog True  <$> lookupECM (roadmapInfo   ctx) ()
    networkH = liftIO $
        lookupECM (networkGraph ctx) ()

    pullsHtmlH = liftIO $ lookupECM (pullInfos ctx) () >>= PullStatus.showPullInfos True
    pullsH = liftIO $ lookupECM (pullInfos ctx) ()

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
app :: ApiContext -> Application
app = serve testApi . server

newApp :: IO Application
newApp = app <$> newContext
