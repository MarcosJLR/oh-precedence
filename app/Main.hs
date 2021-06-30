module Main where

import Components
import Data.Maybe               (fromJust)
import Graph
import LongestPath
import Parser
import Precedence
import Control.Monad.State.Lazy (StateT, get, put, when, unless, liftIO)

import qualified Data.Map.Strict as Map

data ReplState = ReplState
    { rules :: Rules
    , initial :: Symbol
    , precedences :: [Precedence]
    , operators :: [Token]
    , components :: Components
    , lPaths :: LongestPaths
    }

type ReplMonad = StateT ReplState IO

main :: IO ()
main = do
    putStrLn "Hello"


buildEqGraph :: [Precedence] -> Graph
buildEqGraph [] = Map.empty
buildEqGraph ((Equal s t):ps) = addUndirectedEdge ('f':s) ('g':t) $ buildEqGraph ps
buildEqGraph (_:ps) = buildEqGraph ps

buildDag :: Components -> [Precedence] -> Graph
buildDag _ [] = Map.empty
buildDag comps ((Less s t):ps) = addDirectedEdge fs gt $ buildDag comps ps
  where
    fs = fromJust $ Map.lookup ('f':s) comps
    gt = fromJust $ Map.lookup ('f':t) comps
buildDag comps ((Great s t):ps) = addDirectedEdge gt fs $ buildDag comps ps
  where
    fs = fromJust $ Map.lookup ('f':s) comps
    gt = fromJust $ Map.lookup ('f':t) comps
buildDag comps (_:ps) = buildDag comps ps


read' :: ReplMonad String
read' = liftIO $ do
    putStr "oh-prec> "
    getLine

eval' :: String -> ReplMonad String
eval' input =
    case words input of
        [] -> return ""
        ("RULE":args) -> enterRule args
        ("INIT":args) -> initSymbol args
        ("PREC":args) -> insertPrecedence args
        ("BUILD":args) -> buildParser args
        ("PARSE":args) -> useParser args
        _ -> return "Unknown command"


enterRule :: [String] -> ReplMonad String
enterRule [] = return "Rule must have at least right hand non-terminal"
enterRule (nonTerm:rule)
    | isNonTerminal nonTerm = return "Right hand side must be a non-terminal"
    | not (check rule) = return "Rule has two consecutive non-terminals"
    | otherwise = do
        st <- get
        let strRule = nonTerm ++ " -> " ++ unwords rule
        let newRules = Map.insert (concat rule) nonTerm (rules st)
        put $ st {rules = newRules}
        return $ "Rule " ++ strRule ++ " has been added to grammar"
  where
    check [] = True
    check [x] = True
    check (x:y:xs) = isNonTerminal x && isNonTerminal y && check (y:xs)

initSymbol :: [String] -> ReplMonad String
initSymbol [x]
    | isNonTerminal x = do
        st <- get
        put $ st {initial = x}
        return $ x ++ " is now the initial symbol of grammar"
    | otherwise = return $ "ERROR: " ++ x ++ " is not a non-terminal symbol"
initSymbol _ = return "ERROR: One and only one non-terminal symbol must be specified"


insertPrecedence :: [String] -> ReplMonad String
insertPrecedence [x, op, y] = do
    case op of
        "<" -> do
            st <- get
            put $ st {precedences = Less x y : precedences st}
            return $ x ++ " has less precedence than " ++ y
        ">" -> do
            st <- get
            put $ st {precedences = Great x y : precedences st}
            return $ x ++ " has greater precedence than " ++ y
        "=" -> do
            st <- get
            put $ st {precedences = Equal x y : precedences st}
            return $ x ++ " has equal precedence than " ++ y
        _ -> return $ "ERROR: " ++ op ++ " is an invalid operator. Must be one of {<, >, =}"
insertPrecedence _ = return "ERROR: Must specify exactly 3 symbols <terminal> <operator> <terminal>"

buildParser :: [String] -> ReplMonad String
buildParser = undefined

useParser = undefined
