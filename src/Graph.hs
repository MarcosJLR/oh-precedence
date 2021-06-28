module Graph
    ( Graph (..)
    , Key (..)
    , addDirectedEdge
    , addUndirectedEdge
    , adjacencyList
    , removeMultiedges
    , Graph.empty
    )where

import Data.List (nub)
import Data.Map.Strict as Map

type Key = String

type Graph = Map.Map Key [Key]

addDirectedEdge :: Key -> Key -> Graph -> Graph
addDirectedEdge u v = Map.insertWith (++) u [v]

addUndirectedEdge :: Key -> Key -> Graph -> Graph
addUndirectedEdge u v = addDirectedEdge u v . addDirectedEdge v u

removeMultiedges :: Graph -> Graph
removeMultiedges = Map.map nub

empty :: Graph
empty = Map.empty

adjacencyList :: Key -> Graph -> [Key]
adjacencyList v = handle . Map.lookup v
  where
    handle Nothing = []
    handle (Just ks) = ks
