{-# LANGUAGE DataKinds #-}
module GitHub.Tools.Requests where

import           Control.Monad.Catch (throwM)
import           Data.Aeson          (FromJSON, ToJSON (toJSON),
                                      Value (Array, Null, Object))
import qualified Data.Aeson.KeyMap   as KeyMap
import qualified Data.Vector         as V
import qualified GitHub
import           Network.HTTP.Client (Manager)

removeNulls :: ToJSON a => a -> Value
removeNulls = go . toJSON
  where
    go (Array  x) = Array . V.map go $ x
    go (Object x) = Object . KeyMap.map go . KeyMap.filter (not . isEmpty) $ x
    go         x  = x

    isEmpty Null      = True
    isEmpty (Array x) = null x
    isEmpty _         = False

request
  :: FromJSON a
  => Maybe GitHub.Auth
  -> Manager
  -> GitHub.Request 'GitHub.RO a
  -> IO a
request auth mgr req = do
  response <- executeRequest
  case response of
    Left  err -> throwM err
    Right res -> return res

  where
    executeRequest =
      case auth of
        Nothing -> GitHub.executeRequestWithMgr' mgr req
        Just tk -> GitHub.executeRequestWithMgr mgr tk req

mutate
  :: FromJSON a
  => GitHub.Auth
  -> Manager
  -> GitHub.Request 'GitHub.RW a
  -> IO a
mutate auth mgr req = do
  response <- GitHub.executeRequestWithMgr mgr auth req
  case response of
    Left  err -> throwM err
    Right res -> return res
