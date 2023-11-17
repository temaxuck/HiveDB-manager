{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Coordinator.Models.Service where

import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:))
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics

data Service = Service
  { service_name :: String,
    nodes :: [Node],
    sql_tables :: [SqlTable]
  }
  deriving (Show, Generic)

instance FromJSON Service

instance ToJSON Service

data Node = Node
  { ip_address :: String,
    storage :: Storage,
    status :: String
  }
  deriving (Show, Generic)

instance FromJSON Node

instance ToJSON Node

data Storage = Storage
  { total :: Double,
    used :: Double
  }
  deriving (Show, Generic)

instance FromJSON Storage

instance ToJSON Storage

data SqlTable = SqlTable
  { table_name :: String,
    columns :: SqlColumns
  }
  deriving (Show, Generic)

instance FromJSON SqlTable

instance ToJSON SqlTable

type SqlColumns = Map Text Text