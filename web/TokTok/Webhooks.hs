{-# LANGUAGE OverloadedStrings #-}
module TokTok.Webhooks (app) where

import           Control.Applicative    ((<$>))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.CaseInsensitive   as CI
import qualified Data.Maybe             as Maybe
import           Data.Monoid            (mempty, (<>))
import qualified Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.UUID.Types        (UUID)
import           GitHub.Types.Events    (Payload (..))
import           GitHub.WebHook.Handler (Error (..), Handler (..), runHandler)
import           Network.HTTP.Types     (HeaderName, status200, status501)
import           Network.Wai            (Application, Request, Response,
                                         requestBody, requestHeaders,
                                         responseLBS)
import           System.Environment     (lookupEnv)


responseOK :: Response
responseOK = responseLBS status200 [("Content-Type", "text/plain")] "OK"

responseError :: LBS.ByteString -> Response
responseError = responseLBS status501 [("Content-Type", "text/plain")]


showError :: Error -> LBS.ByteString
showError InvalidRequest   = "InvalidRequest"
showError UnsignedRequest  = "UnsignedRequest"
showError InvalidSignature = "InvalidSignature"
showError (ParseError err) =
  "ParseError " <> (LBS.fromStrict . encodeUtf8 . Text.pack . show $ err)


handleError :: BS.ByteString -> [(HeaderName, BS.ByteString)] -> BS.ByteString -> Error -> IO Response
handleError event headers body err = do
  let res = showError err
  putStrLn $ "Failure in event " ++ show event ++ ": "
    ++ Text.unpack (decodeUtf8 (LBS.toStrict res))
  putStrLn $ "- Headers were: " ++ show headers
  putStrLn $ "- Request body was: " ++ Text.unpack (decodeUtf8 body)
  return $ responseError res


handlePayload :: BS.ByteString -> UUID -> Payload -> IO Response
handlePayload event uuid payload = do
  putStrLn $ "Success in event " ++ show event ++ ": UUID=" ++ show uuid
    ++ ", Payload=" ++ show payload
  return responseOK


handleRequest :: BS.ByteString -> String -> Request -> IO Response
handleRequest event secretKey req = do
  body <- fullRequestBody mempty
  parsed <- runHandler Handler
    { hSecretKeys = [secretKey]
    , hBody       = return body
    , hHeader     = return . flip lookup (requestHeaders req) . CI.mk
    }
  case parsed of
    Left err              -> handleError event (requestHeaders req) body err
    Right (uuid, payload) -> handlePayload event uuid payload

  where
    fullRequestBody body = do
      chunk <- requestBody req
      if BS.null chunk
        then return body
        else fullRequestBody $ body <> chunk


app :: Application
app req respond =
  if event `elem` unhandled
    then do
      -- Respond OK if we can't actually handle this type of event.
      putStrLn $ "Ignoring unhandled GitHub event: " ++ show event
      respond responseOK
    else do
      putStrLn $ "Handling GitHub event: " ++ show event
      -- Get the github secret keys.
      secretKeys <- Maybe.maybeToList <$> lookupEnv "GITHUB_SECRET"
      -- If we didn't configure any keys, we just say OK to everything but
      -- don't actually handle any events.
      case secretKeys of
        [secretKey] -> respond =<< handleRequest event secretKey req
        _           -> respond responseOK

  where
    event = Maybe.fromMaybe "" . lookup "X-GitHub-Event" . requestHeaders $ req

    -- The list of unhandled event types. Each of them should have an
    -- associated issue we can track, so we can eventually handle all events.
    unhandled =
      [ ""       -- We ignore the empty string event (no event passed).
      , "ping"   -- https://github.com/wereHamster/github-types/issues/3
      , "status" -- https://github.com/wereHamster/github-types/issues/4
      ]
