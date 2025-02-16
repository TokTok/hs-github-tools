{-# LANGUAGE OverloadedStrings #-}
module GitHub.Types.Json where

import           Data.Aeson        (ToJSON (..), Value (..))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text         as Text
import qualified Data.Vector       as V

removeNulls :: ToJSON a => a -> Value
removeNulls = go . toJSON
  where
    go (Array  x) = Array . V.map go $ x
    go (Object x) = Object . KeyMap.map go . KeyMap.filterWithKey validPair $ x
    go         x  = x

    isEmpty Null      = True
    isEmpty (Array x) = null x
    isEmpty _         = False

    validPair k v = not (isEmpty v || "x-" `Text.isPrefixOf` Key.toText k)

valueIntersection :: Value -> Value -> Value
valueIntersection (Object x) (Object y) = Object $ KeyMap.intersectionWith valueIntersection x y
valueIntersection (Array  x) (Array  y) = Array  $ V.filter (/= Null) $ V.zipWith valueIntersection x y
valueIntersection _ y = y
