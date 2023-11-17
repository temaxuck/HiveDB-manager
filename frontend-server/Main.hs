module Main (main) where

import Frontend.Server (runFrontendServer)
import Infrastructure.Config (loadConfig)

main :: IO ()
main = do
  config <- loadConfig
  runFrontendServer config