{-# LANGUAGE OverloadedStrings #-}
module TokTok.Webhooks (newApp) where

import           Control.Concurrent.MVar    (MVar, modifyMVar_, newMVar,
                                             swapMVar)
import           Control.Concurrent.Suspend (sDelay)
import           Control.Concurrent.Timer   (TimerIO, oneShotRestart,
                                             oneShotTimer)
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.Algorithm.DiffContext as Diff
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.CaseInsensitive       as CI
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text (decodeUtf8With, encodeUtf8)
import qualified Data.Text.Encoding.Error   as Text (lenientDecode)
import qualified Data.Text.IO               as Text
import           Data.UUID                  (UUID)
import           GitHub.Types               (Payload (..))
import           GitHub.WebHook.Handler     (Error (..), Handler (..),
                                             removeNulls, runHandler)
import           Network.HTTP.Types         (HeaderName, status200, status501)
import           Network.Wai                (Application, Request, Response,
                                             getRequestBodyChunk,
                                             requestHeaders, responseLBS)
import           System.Environment         (lookupEnv)
import qualified Text.PrettyPrint           as PP
import           TokTok.Handlers            (handleEvent)


responseOK :: Response
responseOK = responseLBS status200 [("Content-Type", "text/plain")] "OK"

responseError :: LBS.ByteString -> Response
responseError = responseLBS status501 [("Content-Type", "text/plain")]


decodeUtf8 :: BS.ByteString -> Text
decodeUtf8 = Text.decodeUtf8With Text.lenientDecode


showDiff :: Text -> Text -> Text
showDiff a b = Text.pack . PP.render . toDoc $ diff
  where
    toDoc = Diff.prettyContextDiff (PP.text "payload")
                                   (PP.text "value")
                                   (PP.text . Text.unpack)
    diff = Diff.getContextDiff linesOfContext (Text.lines a) (Text.lines b)
    linesOfContext = 3


showError :: Error -> Text
showError InvalidRequest   = "InvalidRequest"
showError UnsignedRequest  = "UnsignedRequest"
showError InvalidSignature = "InvalidSignature"
showError (ParseError err) = "ParseError " <> (Text.pack . show $ err)
showError (IncompleteParse value payload) =
    let a    = json . removeNulls $ payload
        b    = json . removeNulls $ value
        diff = showDiff a b
    in  "IncompleteParse:"
            <> "\n=== Diff ===\n"
            <> diff
            {-
            <> "\n=== Payload ===\n"
            <> a
            <> "\n=== Value ===\n"
            <> b
            -}
    where json = decodeUtf8 . LBS.toStrict . Aeson.encodePretty


handleError
    :: BS.ByteString -> [(HeaderName, BS.ByteString)] -> Error -> IO Response
handleError event headers err = do
    let res = showError err
    Text.putStrLn $ "Failure in event " <> Text.pack (show event) <> ": " <> res
    Text.putStrLn $ "HTTP headers: " <> Text.pack (show headers)
    return . responseError . LBS.fromStrict . Text.encodeUtf8 $ res


handlePayload :: (Payload -> IO Response) -> Bool -> BS.ByteString -> UUID -> Payload -> IO Response
handlePayload f isSigned event uuid payload = do
    Text.putStrLn
        $  unsignedMsg
        <> "Success in event "
        <> Text.pack (show event)
        <> ": UUID="
        <> Text.pack (show uuid)
    f payload
    where unsignedMsg = if isSigned then "" else "[UNSIGNED!] "


handleRequest :: (Payload -> IO Response) -> BS.ByteString -> [String] -> Request -> IO Response
handleRequest f event secretKeys req = do
    body   <- fullRequestBody mempty
    parsed <- runHandler Handler
        { hSecretKeys = secretKeys
        , hBody       = return body
        , hHeader     = return . flip lookup (requestHeaders req) . CI.mk
        }
    case parsed of
        Left err -> handleError event (requestHeaders req) err
        Right (uuid, payload) ->
            handlePayload f (not $ null secretKeys) event uuid payload

  where
    fullRequestBody body = do
        chunk <- getRequestBodyChunk req
        if BS.null chunk then return body else fullRequestBody $ body <> chunk


app :: (Payload -> IO Response) -> Application
app f req respond = case lookup "X-GitHub-Event" . requestHeaders $ req of
    Nothing -> do
        -- Respond OK if we don't get an event.
        putStrLn "Ignoring request without event type"
        respond responseOK
    Just event -> do
        putStrLn $ "Handling GitHub event: " ++ show event
        -- Get the github secret keys.
        secretKeys <- Maybe.maybeToList <$> lookupEnv "GITHUB_SECRET"
        respond =<< handleRequest f event secretKeys req

newApp :: IO Application
newApp = do
    -- Initialise task queue
    tasks <- newMVar Map.empty
    -- Start the task queue processing timer.
    timer <- oneShotTimer (processTasks tasks) (sDelay 30)  -- after 30 seconds
    return $ app (addTasks timer tasks)
  where
    processTasks :: MVar (Map Text (IO ())) -> IO ()
    processTasks tasks = do
        todo <- swapMVar tasks Map.empty
        putStrLn $ "processing " <> show (Map.size todo) <> " background tasks"
        sequence_ $ Map.elems todo

    addTasks :: TimerIO -> MVar (Map Text (IO ())) -> Payload -> IO Response
    addTasks timer tasks payload =
        case handleEvent payload of
            Nothing -> return responseOK
            Just (key, task) -> do
                modifyMVar_ tasks $ return . Map.insert key task
                success <- oneShotRestart timer
                return $ if success
                    then responseOK
                    else responseError "failed to start background processing task"
