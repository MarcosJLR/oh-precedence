module Components where

import Graph
import Control.Monad.State.Lazy (State, get, put, runState)
import qualified Data.Map.Strict as Map

type Components = Map.Map Key Key

data DfsState = DfsState
    { components :: Components
    , sizes :: Map.Map Key Int
    }

type DfsMonad = State DfsState

initState :: DfsState
initState = DfsState Map.empty Map.empty

connectedComponents :: Graph -> Either String Components
connectedComponents graph = checkEquivalence graph finalState
  where
    loop [] = return ()
    loop (k:ks) = do
        dfsComponents graph k k
        loop ks
    (_, finalState) = runState (loop $ Map.keys graph) initState


dfsComponents :: Graph -> Key -> Key -> DfsMonad ()
dfsComponents graph root v = do
    st <- get
    let cmp = components st
    let sz = sizes st
    let mComp = Map.lookup v cmp
    case mComp of
        Just _ -> return ()
        Nothing -> do
            let cmp' = Map.insert v root cmp
            let sz' = Map.adjust (+1) root sz
            put $ st {components = cmp', sizes = sz'}
            let adj = adjacencyList v graph
            mapM_ (dfsComponents graph root) adj

checkEquivalence :: Graph -> DfsState -> Either String Components
checkEquivalence graph state = do
    let cmps = components state
    let szs = sizes state
    let check key = do
        let mComp = Map.lookup key cmps
        case mComp of
            Nothing -> Left $ "Key " ++ key ++ " does not have component"
            Just comp -> do
                let mSize = Map.lookup comp szs
                case mSize of
                    Nothing -> Left $ "Component " ++ comp ++ " does not have size"
                    Just sz ->
                        if sz-1 == length (adjacencyList key graph)
                        then return ()
                        else Left $ "Key " ++ key ++ " does not connect to every node in its component"

    mapM_ check $ Map.keys cmps
    return cmps
