{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Applicative      ((<|>))
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (Port, run)
import           Network.Wai.UrlMap       (mapUrls, mount)
import           System.Environment       (getArgs)
import           System.IO                (BufferMode (..), hSetBuffering,
                                           stdout)

import qualified TokTok.Hello             as Hello
import qualified TokTok.Webhooks          as Webhooks


app :: Application
app = mapUrls $
      mount "hello" Hello.app
  <|> mount "webhooks" Webhooks.app


-- Run the server.
runTestServer :: Port -> IO ()
runTestServer port = run port app

-- Put this all to work!
main :: IO ()
main = do
  -- So real time logging works correctly.
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    [port] -> runTestServer $ read port
    _      -> runTestServer 8001
