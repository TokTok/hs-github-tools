{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative         ((<|>))
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (Port, run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Network.Wai.UrlMap          (mapUrls, mount)
import           System.Environment          (getArgs, getEnv)
import           System.IO                   (BufferMode (..), hSetBuffering,
                                              stdout)

import qualified TokTok.Hello                as Hello
import qualified TokTok.Webhooks             as Webhooks


newApp :: IO Application
newApp = do
    helloApp <- simpleCors <$> Hello.newApp
    return $ mapUrls $
            mount "hello" helloApp
        <|> mount "webhooks" Webhooks.app


-- Run the server.
runTestServer :: Port -> IO ()
runTestServer port = do
    putStrLn $ "Running webserver on port " ++ show port
    run port =<< newApp

-- Put this all to work!
main :: IO ()
main = do
  -- So real time logging works correctly.
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case args of
        [port] -> runTestServer $ read port
        _      -> runTestServer =<< read <$> getEnv "PORT"
