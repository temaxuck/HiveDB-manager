module Frontend.Views where

import Backend.Coordinator.Database.Redis
  ( getServices,
  )
import Backend.Coordinator.Models.Service
import Control.Monad (foldM)
import Database.Redis
  ( Connection,
  )

countActiveNodes :: [Service] -> (Int, Int)
countActiveNodes services = foldl count (0, 0) services
  where
    count (total, active) service =
      let nodes_ = nodes service
          total' = total + length nodes_
          active' = active + length (filter (\node -> status node == "active") nodes_)
       in (total', active')

countActiveNodesForService :: Service -> (Int, Int)
countActiveNodesForService service =
  let nodes_ = nodes service
      total = length nodes_
      active = length (filter (\node -> status node == "active") nodes_)
   in (total, active)

countUsedStorage :: [Service] -> (Int, Int)
countUsedStorage services =
  let nodes_ = concatMap nodes services
      totalUsedStorage = sum (map (\node -> used (storage node)) nodes_)
      totalStorage = sum (map (\node -> total (storage node)) nodes_)
   in (round totalUsedStorage :: Int, round totalStorage :: Int)

countUsedStorageForService :: Service -> (Int, Int)
countUsedStorageForService service =
  let nodes_ = nodes service
      totalUsedStorage = sum (map (\node -> used (storage node)) nodes_)
      totalStorage = sum (map (\node -> total (storage node)) nodes_)
   in (round totalUsedStorage :: Int, round totalStorage :: Int)
