{-# LANGUAGE OverloadedStrings #-}
module TokTok.Webhooks (app) where

import           Control.Applicative    ((<$>))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.CaseInsensitive   as CI
import qualified Data.Maybe             as Maybe
import           Data.Monoid            (mempty, (<>))
import qualified Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.UUID              (UUID)
import           GitHub.Types           (Payload (..))
import           GitHub.WebHook.Handler (Error (..), Handler (..), removeNulls,
                                         runHandler)
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
showError (IncompleteParse value payload) =
  "IncompleteParse:"
  <> "\n- Value: "   <> Aeson.encode (removeNulls value  )
  <> "\n- Payload: " <> Aeson.encode (removeNulls payload)


handleError :: BS.ByteString -> [(HeaderName, BS.ByteString)] -> BS.ByteString -> Error -> IO Response
handleError event headers body err = do
  let res = showError err
  putStrLn $ "Failure in event " ++ show event ++ ": "
    ++ Text.unpack (decodeUtf8 (LBS.toStrict res))
  putStrLn $ "- Headers were: " ++ show headers
  putStrLn $ "- Request body was: " ++ Text.unpack (decodeUtf8 body)
  return $ responseError res


handlePayload :: Bool -> BS.ByteString -> UUID -> Payload -> IO Response
handlePayload isSigned event uuid _payload = do
  putStrLn $ unsignedMsg ++ "Success in event " ++ show event ++ ": UUID=" ++ show uuid
  return responseOK
  where
    unsignedMsg = if isSigned then "" else "[UNSIGNED!] "


handleRequest :: BS.ByteString -> [String] -> Request -> IO Response
handleRequest event secretKeys req = do
  body <- fullRequestBody mempty
  parsed <- runHandler Handler
    { hSecretKeys = secretKeys
    , hBody       = return body
    , hHeader     = return . flip lookup (requestHeaders req) . CI.mk
    }
  case parsed of
    Left err              -> handleError event (requestHeaders req) body err
    Right (uuid, payload) -> handlePayload (not $ null secretKeys) event uuid payload

  where
    fullRequestBody body = do
      chunk <- requestBody req
      if BS.null chunk
        then return body
        else fullRequestBody $ body <> chunk


app :: Application
app req respond =
  case lookup "X-GitHub-Event" . requestHeaders $ req of
    Nothing -> do
      -- Respond OK if we don't get an event.
      putStrLn $ "Ignoring request without event type"
      respond responseOK
    Just event -> do
      putStrLn $ "Handling GitHub event: " ++ show event
      -- Get the github secret keys.
      secretKeys <- Maybe.maybeToList <$> lookupEnv "GITHUB_SECRET"
      respond =<< handleRequest event secretKeys req
