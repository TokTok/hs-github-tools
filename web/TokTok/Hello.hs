{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module TokTok.Hello (newApp) where

import           Caching.ExpiringCacheMap.HashECM (CacheSettings (..), ECM,
                                                   consistentDuration,
                                                   lookupECM, newECMIO)
import           Control.Applicative              ((<$>), (<*>))
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
#if !MIN_VERSION_servant_server(0, 6, 0)
import           Network.Wai                      (Application)
#endif
import           Servant
import           System.Environment               (getEnv)

import qualified Changelogs
import           PullRequestInfo                  (PullRequestInfo)
import qualified PullStatus


type GitHubCache a = ECM IO MVar () HashMap () a


data ApiContext = ApiContext
  { changelogInfo :: GitHubCache Changelogs.ChangeLog
  , roadmapInfo   :: GitHubCache Changelogs.ChangeLog
  , pullInfos     :: GitHubCache [[PullRequestInfo]]
  }


newGitHubCache :: Int -> IO a -> IO (GitHubCache a)
newGitHubCache timeout fetchUpdates =
  newECMIO
    (consistentDuration timeout $ \state () -> do
      infos <- fetchUpdates
      return (state, infos))
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
  where
    sourceH = return "https://github.com/TokTok/github-tools"

    changelogH = liftIO $
      Changelogs.formatChangeLog False <$> lookupECM (changelogInfo ctx) ()
    roadmapH = liftIO $
      Changelogs.formatChangeLog True  <$> lookupECM (roadmapInfo   ctx) ()

    pullsHtmlH = liftIO $ lookupECM (pullInfos ctx) () >>= PullStatus.showPullInfos True
    pullsH = liftIO $ lookupECM (pullInfos ctx) ()

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
app :: ApiContext -> Application
app = serve testApi . server

newApp :: IO Application
newApp = app <$> newContext
