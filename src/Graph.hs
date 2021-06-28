module Graph where

import Data.List (nub)
import Data.Map as Map

type Key = String

type Graph = Map Key [Key]

addDirectedEdge :: Key -> Key -> Graph -> Graph
addDirectedEdge u v = Map.insertWith (++) u [v]

addUndirectedEdge :: Key -> Key -> Graph -> Graph
addUndirectedEdge u v = addDirectedEdge u v . addDirectedEdge v u

removeMultiedges :: Graph -> Graph
removeMultiedges = Map.map nub
