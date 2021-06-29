module LongestPath
    ( LongestPaths
    , findLongestPaths
    , getLPath
    ) where

import Graph
import Control.Monad.State.Lazy (State, get, put, runState, when)
import Components               (Components)

import qualified Data.Map.Strict as Map

type LongestPaths = Map.Map Key Int

type DagMonad a = State LongestPaths a

findLongestPaths :: Graph -> [Key] -> LongestPaths
findLongestPaths graph keys = finalState
  where
    loop [] = return 0
    loop (k:ks) = do
        longestPathFrom graph k
        loop ks
    (_, finalState) = runState (loop keys) Map.empty

longestPathFrom :: Graph -> Key -> DagMonad Int
longestPathFrom graph v = do
    st <- get
    let mLPath = Map.lookup v st
    case mLPath of
        Just x ->
            if x == -1
            then error "ERROR: A cycle has been found"
            else return x
        Nothing -> do
            put $ Map.insert v (-1) st
            paths <- mapM (longestPathFrom graph) (adjacencyList v graph)
            let lPath = 1 + foldr max 0 paths
            put $ Map.insert v lPath st
            return lPath

getLPath :: LongestPaths -> Key -> Maybe Int
getLPath = flip Map.lookup
