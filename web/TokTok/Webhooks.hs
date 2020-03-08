{-# LANGUAGE OverloadedStrings #-}
module TokTok.Webhooks (app) where

import           Control.Applicative        ((<$>))
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.Algorithm.DiffContext as Diff
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.CaseInsensitive       as CI
import qualified Data.Maybe                 as Maybe
import           Data.Monoid                (mempty, (<>))
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


responseOK :: Response
responseOK = responseLBS status200 [("Content-Type", "text/plain")] "OK"

responseError :: LBS.ByteString -> Response
responseError = responseLBS status501 [("Content-Type", "text/plain")]


decodeUtf8 :: BS.ByteString -> Text
decodeUtf8 = Text.decodeUtf8With Text.lenientDecode


showDiff :: Text -> Text -> Text
showDiff a b = Text.pack . PP.render . toDoc $ diff
  where
    toDoc = Diff.prettyContextDiff (PP.text "payload") (PP.text "value") (PP.text . Text.unpack)
    diff = Diff.getContextDiff linesOfContext (Text.lines a) (Text.lines b)
    linesOfContext = 3


showError :: Error -> Text
showError InvalidRequest   = "InvalidRequest"
showError UnsignedRequest  = "UnsignedRequest"
showError InvalidSignature = "InvalidSignature"
showError (ParseError err) =
  "ParseError " <> (Text.pack . show $ err)
showError (IncompleteParse value payload) =
  let
    a = json . removeNulls $ payload
    b = json . removeNulls $ value
    diff = showDiff a b
  in
  "IncompleteParse:"
  <> "\n=== Diff ===\n"    <> diff
--  <> "\n=== Payload ===\n" <> a
--  <> "\n=== Value ===\n"   <> b
  where
    json = decodeUtf8 . LBS.toStrict . Aeson.encodePretty


handleError :: BS.ByteString -> [(HeaderName, BS.ByteString)] -> Error -> IO Response
handleError event headers err = do
  let res = showError err
  Text.putStrLn $ "Failure in event " <> Text.pack (show event) <> ": " <> res
  Text.putStrLn $ "HTTP headers: " <> Text.pack (show headers)
  return . responseError . LBS.fromStrict . Text.encodeUtf8 $ res


handlePayload :: Bool -> BS.ByteString -> UUID -> Payload -> IO Response
handlePayload isSigned event uuid _payload = do
  Text.putStrLn $ unsignedMsg <> "Success in event " <> Text.pack (show event) <> ": UUID=" <> Text.pack (show uuid)
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
    Left err              -> handleError event (requestHeaders req) err
    Right (uuid, payload) -> handlePayload (not $ null secretKeys) event uuid payload

  where
    fullRequestBody body = do
      chunk <- getRequestBodyChunk req
      if BS.null chunk
        then return body
        else fullRequestBody $ body <> chunk


app :: Application
app req respond =
  case lookup "X-GitHub-Event" . requestHeaders $ req of
    Nothing -> do
      -- Respond OK if we don't get an event.
      putStrLn "Ignoring request without event type"
      respond responseOK
    Just event -> do
      putStrLn $ "Handling GitHub event: " ++ show event
      -- Get the github secret keys.
      secretKeys <- Maybe.maybeToList <$> lookupEnv "GITHUB_SECRET"
      respond =<< handleRequest event secretKeys req
