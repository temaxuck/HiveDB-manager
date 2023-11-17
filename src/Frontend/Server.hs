{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Server (runFrontendServer) where

import Backend.Coordinator.Database.Redis
  ( addNode,
    addService,
    getDatabaseConnection,
    getServices,
  )
import Backend.Coordinator.Models.Service
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)
import Frontend.Views
import Infrastructure.Config (Config (..))
import Text.Blaze.Renderer.String (renderMarkup)
import Text.Heterocephalus (compileHtmlFile)
import Web.Scotty (get, html, param, post, redirect, scotty)
import Web.Scotty.Login.Session
  ( addSession,
    authCheck,
    initializeCookieDb,
    removeSession,
  )

runFrontendServer :: Config -> IO ()
runFrontendServer config = do
  initializeCookieDb $ _sessionConfig config
  redisConnection <- getDatabaseConnection config
  scotty (_frontendServerPort config) $
    do
      -- ==== Authentication routes ====
      get "/login"
        $ authCheck
          ( html . pack $
              renderMarkup
                ( let clusterName = _clusterName config :: String
                   in $(compileHtmlFile "static/templates/general/login.html")
                )
          )
        $ redirect "/status"
      post "/login" $
        do
          username <- param "username"
          password <- param "password"
          if username == ("admin" :: String) && password == ("admin" :: String)
            then do
              _ <- addSession $ _sessionConfig config
              redirect "/status"
            else do
              redirect "/login"
      get "/logout" $
        do
          _ <- removeSession $ _sessionConfig config
          redirect "/login"

      -- ==== Hive DB routes ====
      get "/" $ authCheck (redirect "/login") $ redirect "/status"
      get "/status" $
        authCheck (redirect "/login") $
          do
            _services <- liftIO $ getServices redisConnection
            html . pack $
              renderMarkup
                ( let clusterName = _clusterName config :: String
                      services = _services :: [Service]
                   in $(compileHtmlFile "static/templates/status/status.html")
                )
      get "/cluster" $
        authCheck (redirect "/login") $
          redirect "/cluster/add-new-service"
      get "/cluster/add-new-service" $
        authCheck (redirect "/login") $
          do
            html . pack $
              renderMarkup
                ( let clusterName = _clusterName config :: String
                   in $(compileHtmlFile "static/templates/cluster/add_service.html")
                )
      post "/cluster/add-new-service" $
        authCheck (redirect "/login") $
          do
            serviceName <- param "service_name"
            liftIO $ addService redisConnection serviceName
            redirect "/cluster/add-new-service"
      get "/cluster/add-new-server" $
        authCheck (redirect "/login") $
          do
            _services <- liftIO $ getServices redisConnection
            html . pack $
              renderMarkup
                ( let clusterName = _clusterName config :: String
                      services = _services :: [Service]
                   in $(compileHtmlFile "static/templates/cluster/add_node.html")
                )
      post "/cluster/add-new-server" $
        authCheck (redirect "/login") $
          do
            serviceName <- param "service_name"
            ipAddress <- param "ip_address"
            liftIO $ addNode redisConnection serviceName ipAddress
            redirect "/cluster/add-new-server"
      get "/data" $ authCheck (redirect "/login") $ redirect "/data/get"
      get "/data/get" $
        authCheck (redirect "/login") $
          do
            _services <- liftIO $ getServices redisConnection
            html . pack $
              renderMarkup
                ( let clusterName = _clusterName config :: String
                      services = _services :: [Service]
                   in $(compileHtmlFile "static/templates/data/get.html")
                )
      post "/data/get" $
        authCheck (redirect "/login") $
          redirect "/data/get"
      get "/data/post" $
        authCheck (redirect "/login") $
          do
            _services <- liftIO $ getServices redisConnection
            html . pack $
              renderMarkup
                ( let clusterName = _clusterName config :: String
                      services = _services :: [Service]
                   in $(compileHtmlFile "static/templates/data/post.html")
                )
      post "/data/post" $
        authCheck (redirect "/login") $
          redirect "/data/post"
      get "/data/patch" $
        authCheck (redirect "/login") $
          do
            _services <- liftIO $ getServices redisConnection
            html . pack $
              renderMarkup
                ( let clusterName = _clusterName config :: String
                      services = _services :: [Service]
                   in $(compileHtmlFile "static/templates/data/patch.html")
                )
      post "/data/patch" $
        authCheck (redirect "/login") $
          redirect "/data/patch"
      get "/data/delete" $
        authCheck (redirect "/login") $
          do
            _services <- liftIO $ getServices redisConnection
            html . pack $
              renderMarkup
                ( let clusterName = _clusterName config :: String
                      services = _services :: [Service]
                   in $(compileHtmlFile "static/templates/data/delete.html")
                )
      post "/data/delete" $
        authCheck (redirect "/login") $
          redirect "/data/delete"