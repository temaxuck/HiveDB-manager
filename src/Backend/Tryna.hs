{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Tryna (tryna) where

import Backend.Coordinator.Database.Redis
  ( getDatabaseConnection,
    getNodes,
    getServiceNames,
    getSqlTables,
  )
import Backend.Coordinator.Models.Service
  ( Node (..),
    SqlColumns,
    SqlTable,
    Storage (..),
  )
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Database.Redis hiding (decode)
import GHC.Generics
import Infrastructure.Config (Config (..))

deserializeSqlColumns :: B.ByteString -> Maybe SqlColumns
deserializeSqlColumns sqlColumnsJsonStr = decode sqlColumnsJsonStr :: Maybe SqlColumns

serializeServiceNames :: [String] -> B.ByteString
serializeServiceNames = encode

serializeNodes :: [Node] -> B.ByteString
serializeNodes = encode

tryna :: Config -> IO ()
tryna config = do
  redisConnection <- getDatabaseConnection config
  -- serviceNames <- getServiceNames redisConnection
  nodes_ <- getNodes redisConnection "news"
  let newNode =
        Node
          { ip_address = "duaug",
            storage =
              Storage
                { total = 0,
                  used = 0
                },
            status = "inactive"
          }
  let key = C.pack "test"
  let value = BS.toStrict . serializeNodes $ [newNode] ++ nodes_
  _ <- runRedis redisConnection $ set key value
  return ()
