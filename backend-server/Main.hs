module Main (main) where

import Backend.Server (runBackendServer)
import Infrastructure.Config (loadConfig)

main :: IO ()
main = do
  config <- loadConfig
  runBackendServer config

