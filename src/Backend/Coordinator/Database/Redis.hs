{-# LANGUAGE OverloadedStrings #-}

module Backend.Coordinator.Database.Redis where

import Backend.Coordinator.Models.Service
import Control.Monad (forM)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Database.Redis
  ( ConnectInfo (..),
    Connection,
    connect,
    get,
    keys,
    parseConnectInfo,
    runRedis,
    set,
  )
import Infrastructure.Config (Config (..))

getDatabaseConnection :: Config -> IO Connection
getDatabaseConnection config = do
  let eitherConnectInfo = parseConnectInfo $ _redisUri config
  case eitherConnectInfo of
    Left errorMessage -> error $ "Error parsing connection info: " ++ errorMessage
    Right connectInfo -> do
      connect connectInfo

deserializeServiceNames :: B.ByteString -> Maybe [String]
deserializeServiceNames serviceNamesJsonStr = decode serviceNamesJsonStr :: Maybe [String]

serializeServiceNames :: [String] -> B.ByteString
serializeServiceNames = encode

getServiceNames :: Connection -> IO [String]
getServiceNames connection = do
  eitherServiceNamesJsonStr <- runRedis connection $ get "services"
  case eitherServiceNamesJsonStr of
    Left _ -> return []
    Right mbServiceNamesJsonStr -> case mbServiceNamesJsonStr of
      Nothing -> return []
      Just serviceNamesJsonStr -> case deserializeServiceNames $ B.fromStrict serviceNamesJsonStr of
        Nothing -> return []
        Just serviceNames -> return serviceNames

deserializeNodes :: B.ByteString -> Maybe [Node]
deserializeNodes nodesJsonStr = decode nodesJsonStr :: Maybe [Node]

serializeNodes :: [Node] -> B.ByteString
serializeNodes = encode

getNodes :: Connection -> String -> IO [Node]
getNodes connection serviceName = do
  eitherNodesJsonStr <- runRedis connection $ get $ C.pack (serviceName ++ ":nodes")

  case eitherNodesJsonStr of
    Left _ -> return []
    Right mbNodesJsonStr -> case mbNodesJsonStr of
      Nothing -> return []
      Just nodesJsonStr -> case deserializeNodes $ B.fromStrict nodesJsonStr of
        Nothing -> return []
        Just nodes -> return nodes

deserializeSqlColumns :: B.ByteString -> Maybe SqlColumns
deserializeSqlColumns sqlColumnsJsonStr = decode sqlColumnsJsonStr :: Maybe SqlColumns

getSqlTables :: Connection -> String -> IO [SqlTable]
getSqlTables connection serviceName = do
  let pattern = C.pack $ serviceName ++ ":sql_table:*"
  eitherSqlTableKeys <- runRedis connection $ keys pattern
  case eitherSqlTableKeys of
    Left _ -> return [] -- error occured
    Right sqlTableKeys -> do
      sqlTables <- forM sqlTableKeys $ \sqlTableKey -> do
        let tableName = last $ splitOn ":" (C.unpack sqlTableKey)
        eitherSqlColumnsJsonStr <- runRedis connection $ get sqlTableKey
        case eitherSqlColumnsJsonStr of
          Left _ -> return Nothing
          Right mbSqlColumnsJsonStr -> case mbSqlColumnsJsonStr of
            Nothing -> return Nothing
            Just sqlColumnsJsonStr -> case deserializeSqlColumns $ B.fromStrict sqlColumnsJsonStr of
              Nothing -> return Nothing
              Just sqlColumns -> return $ Just $ SqlTable tableName sqlColumns
      return $ catMaybes sqlTables

getServices :: Connection -> IO [Service]
getServices connection = do
  serviceNames <- getServiceNames connection
  services <- forM serviceNames $ \_serviceName -> do
    _nodes <- getNodes connection _serviceName
    _sqlTables <- getSqlTables connection _serviceName
    return $
      Service
        { service_name = _serviceName,
          nodes = _nodes,
          sql_tables = _sqlTables
        }

  return services

addService :: Connection -> String -> IO Bool
addService connection serviceName = do
  serviceNames <- getServiceNames connection
  let key = C.pack "services"
  let value = BS.toStrict . serializeServiceNames $ [serviceName] ++ serviceNames
  setResult <- runRedis connection $ set key value
  case setResult of
    Left _ -> return False
    Right _ -> return True

addNode :: Connection -> String -> String -> IO Bool
addNode connection serviceName ipAddress = do
  serviceNames <- getServiceNames connection
  if serviceName `elem` serviceNames
    then do
      nodes_ <- getNodes connection serviceName
      let newNode =
            Node
              { ip_address = ipAddress,
                storage =
                  Storage
                    { total = 0,
                      used = 0
                    },
                status = "uninitialized"
              }
      let key = C.pack $ serviceName ++ ":nodes"
      let value = BS.toStrict . serializeNodes $ [newNode] ++ nodes_
      setResult <- runRedis connection $ set key value
      case setResult of
        Left _ -> return False
        Right _ -> return True
    else return False
