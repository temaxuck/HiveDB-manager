module Infrastructure.Config (loadConfig, Config (..)) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime)
import System.Environment (lookupEnv)
import Web.Scotty.Login.Session (SessionConfig (..), defaultSessionConfig)

type ClusterName = String

data Config = Config
  { -- Common config values
    _clusterName :: ClusterName,
    -- frontend
    _frontendServerPort :: Int,
    _sessionConfig :: SessionConfig,
    -- backend
    _backendServerPort :: Int,
    _redisUri :: String
  }

loadConfig :: IO Config
loadConfig = do
  loadFile defaultConfig

  clusterName <- fromMaybe "Set CLUSTER_NAME var" <$> lookupEnv "CLUSTER_NAME"
  sessionExpirationTimeStr <- fromMaybe "600" <$> lookupEnv "SESSION_EXPIRATION_TIME"
  sessionDBLocation <- fromMaybe "data/sessions.sqlite3" <$> lookupEnv "SESSION_DB_PATH"
  frontendServerPortStr <- fromMaybe "8080" <$> lookupEnv "FRONTEND_SERVER_PORT"
  backendServerPortStr <- fromMaybe "6913" <$> lookupEnv "BACKEND_SERVER_PORT"
  redisUri <- fromMaybe "redis://" <$> lookupEnv "REDIS_URL"

  let sessionExpirationTime = fromIntegral (read sessionExpirationTimeStr :: Integer) :: NominalDiffTime
      frontendServerPort = read frontendServerPortStr :: Int
      backendServerPort = read backendServerPortStr :: Int
      sessionConfig =
        defaultSessionConfig
          { expirationInterval = sessionExpirationTime,
            dbPath = sessionDBLocation
          }

  return
    Config
      { _clusterName = clusterName,
        _frontendServerPort = frontendServerPort,
        _sessionConfig = sessionConfig,
        _backendServerPort = backendServerPort,
        _redisUri = redisUri
      }
