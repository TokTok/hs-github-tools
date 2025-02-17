{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative         ((<|>))
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (Port, run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Network.Wai.UrlMap          (mapUrls, mount)
import           System.Environment          (getArgs, lookupEnv)
import           System.IO                   (BufferMode (..), hSetBuffering,
                                              stdout)

import qualified TokTok.Hello                as Hello
import qualified TokTok.Webhooks             as Webhooks


newApp :: IO Application
newApp = do
    hello <- simpleCors <$> Hello.newApp
    webhooks <- Webhooks.newApp
    return $ mapUrls $
            mount "hello" hello
        <|> mount "webhooks" webhooks


-- Run the server.
runTestServer :: Port -> IO ()
runTestServer port = do
    putStrLn $ "Running webserver on port " ++ show port
    run port =<< newApp

getPort :: IO Port
getPort =
    -- Prefer $PORT, otherwise the first command line argument, otherwise 8080.
    lookupEnv "PORT" >>= \case
        Just port -> return $ read port
        Nothing -> do
            args <- getArgs
            return $ case args of
                [port] -> read port
                _      -> 8080

-- Put this all to work!
main :: IO ()
main = do
    -- So real time logging works correctly.
    hSetBuffering stdout LineBuffering
    getPort >>= runTestServer
