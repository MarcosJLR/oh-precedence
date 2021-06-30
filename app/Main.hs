module Main where

import Components
import Data.Maybe               (fromJust)
import Data.List                (nub)
import Graph
import LongestPath
import Parser
import Precedence
import Control.Monad.State.Lazy  (StateT, get, put, when, unless, liftIO, evalStateT)
import Control.Monad.Writer.Lazy (runWriter)
import System.IO                 (hFlush, stdout)

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

initialState :: ReplState
initialState = ReplState
    { rules = Map.empty
    , initial = undefined
    , precedences = []
    , operators = []
    , components = Map.empty
    , lPaths = Map.empty
    }

main :: IO ()
main = evalStateT loop initialState
  where
    loop :: ReplMonad ()
    loop = do
        input <- read'
        unless (input == "EXIT") $ do
            output <- eval' input
            print' output
            loop


buildEqGraph :: [Precedence] -> Graph
buildEqGraph [] = Map.empty
buildEqGraph ((Equal s t):ps) = addUndirectedEdge ('f':s) ('g':t) $ buildEqGraph ps
buildEqGraph (_:ps) = buildEqGraph ps

buildDag :: Components -> [Precedence] -> Graph
buildDag _ [] = Map.empty
buildDag comps ((Less s t):ps) = addDirectedEdge gt fs $ buildDag comps ps
  where
    fs = fromJust $ Map.lookup ('f':s) comps
    gt = fromJust $ Map.lookup ('g':t) comps
buildDag comps ((Great s t):ps) = addDirectedEdge fs gt $ buildDag comps ps
  where
    fs = fromJust $ Map.lookup ('f':s) comps
    gt = fromJust $ Map.lookup ('g':t) comps
buildDag comps (_:ps) = buildDag comps ps


read' :: ReplMonad String
read' = do
    liftIO $ putStr "oh-prec> "
    liftIO $ hFlush stdout
    liftIO getLine

eval' :: String -> ReplMonad String
eval' input =
    case words input of
        [] -> return ""
        ("RULE":args) -> enterRule args
        ("INIT":args) -> initSymbol args
        ("PREC":args) -> insertPrecedence args
        ("BUILD":args) -> buildParser args
        ("PARSE":args) -> useParser args
        _ -> return "ERROR: Unknown command"

print' :: String -> ReplMonad ()
print' = liftIO . putStrLn


enterRule :: [String] -> ReplMonad String
enterRule [] = return "Rule must have at least right hand non-terminal"
enterRule (nonTerm:rule)
    | not (isNonTerminal nonTerm) = return "Right hand side must be a non-terminal"
    | checkBad rule = return "Rule has two consecutive non-terminals"
    | otherwise = do
        st <- get
        let strRule = nonTerm ++ " -> " ++ unwords rule
        let newRules = Map.insert (concat rule) nonTerm (rules st)
        put $ st {rules = newRules}
        return $ "Rule " ++ strRule ++ " has been added to grammar"
  where
    checkBad [] = False
    checkBad [x] = False
    checkBad (x:y:xs) = (isNonTerminal x && isNonTerminal y) || checkBad (y:xs)

initSymbol :: [String] -> ReplMonad String
initSymbol [x]
    | isNonTerminal x = do
        st <- get
        put $ st {initial = x}
        return $ x ++ " is now the initial symbol of grammar"
    | otherwise = return $ "ERROR: " ++ x ++ " is not a non-terminal symbol"
initSymbol _ = return "ERROR: One and only one non-terminal symbol must be specified"


insertPrecedence :: [String] -> ReplMonad String
insertPrecedence [x, op, y]
    | isNonTerminal x || isNonTerminal y = return "ERROR: Use only terminal symbols to parse"
    | otherwise = do
        st <- get
        let newOps = nub $ x : y : operators st
        case op of
            "<" -> do
                put $ st {precedences = Less x y : precedences st, operators = newOps}
                return $ x ++ " has less precedence than " ++ y
            ">" -> do
                put $ st {precedences = Great x y : precedences st, operators = newOps}
                return $ x ++ " has greater precedence than " ++ y
            "=" -> do
                put $ st {precedences = Equal x y : precedences st, operators = newOps}
                return $ x ++ " has equal precedence than " ++ y
            _ -> return $ "ERROR: " ++ op ++ " is an invalid operator. Must be one of {<, >, =}"
insertPrecedence _ = return "ERROR: Must specify exactly 3 symbols <terminal> <operator> <terminal>"

buildParser :: [String] -> ReplMonad String
buildParser (_:_) = return "ERROR: BUILD command must be used without arguments"
buildParser [] = do
    st <- get
    let eqGraph = removeMultiedges $ buildEqGraph $ precedences st
    let ops = operators st
    let keys = map ('f':) ops ++ map ('g':) ops
    case connectedComponents eqGraph keys of
        Left err -> return $ "ERROR: " ++ err
        Right comps -> do
            let dag = removeMultiedges $ buildDag comps $ precedences st
            let longest = findLongestPaths dag keys
            liftIO $ print dag
            put $ st {components = comps, lPaths = longest}
            let fValues = unlines $ map (showValue comps longest "f") $ operators st
            let gValues = unlines $ map (showValue comps longest "g") $ operators st
            return $ unlines ["f-values:", fValues, "g-values", gValues]
  where
    showValue :: Components -> LongestPaths -> String -> Token -> String
    showValue comps longest fg tk = "- " ++ fg ++ "(" ++ tk ++ "): " ++ show value
      where
        cmp = fromJust $ Map.lookup (fg ++ tk) comps
        value = fromJust $ Map.lookup cmp longest

useParser :: [String] -> ReplMonad String
useParser tks = do
    st <- get
    let init = initial st
    let rs = rules st
    let comps = components st
    let longest = lPaths st
    let (result, log) = runWriter $ parse init rs comps longest tks
    return $ unlines log

