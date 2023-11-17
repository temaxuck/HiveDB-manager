{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Server (runBackendServer) where

import Backend.Coordinator.Database.Redis
  ( -- getNodes,
    -- getServiceNames,
    -- getSqlTables,

    addNode,
    addService,
    getDatabaseConnection,
    getServices,
  )
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (pack)
import Infrastructure.Config (Config (..))
import Web.Scotty (get, json, param, post, redirect, scotty)

runBackendServer :: Config -> IO ()
runBackendServer config = do
  redisConnection <- getDatabaseConnection config
  scotty (_backendServerPort config) $
    do
      get "/" $ do
        json $ object ["message" .= ("This is HiveDB server!" :: String)]
      get "/services" $ do
        services <- liftIO $ getServices redisConnection
        json services
      post "/services" $ do
        serviceName <- param "service_name"
        isAdded <- liftIO $ addService redisConnection serviceName
        if isAdded
          then json $ object ["status" .= ("Service added successfully" :: String)]
          else json $ object ["status" .= (("Couldn't add service" ++ serviceName) :: String)]
      post "/services/:service_name/add-server" $ do
        serviceName <- param "service_name"
        ipAddress <- param "ip_address"
        isAdded <- liftIO $ addNode redisConnection serviceName ipAddress
        if isAdded
          then json $ object ["status" .= ("Server added successfully" :: String)]
          else json $ object ["status" .= (("Couldn't add server for " ++ serviceName ++ " with ip address " ++ ipAddress) :: String)]
