module Parser
    ( Token
    , Symbol
    , Rules
    , parse
    , isNonTerminal
    ) where

import Data.Char                    (isUpper)
import Data.Maybe                   (fromJust)
import Data.Map.Strict              (notMember)
import Control.Monad.Writer.Lazy    (Writer, tell, when, unless)
import Components                   (Components, getComponent)
import LongestPath                  (LongestPaths, getLPath)
import Precedence                   (Symbol, Rules, Token)

import qualified Data.Map.Strict as Map


type ParseMonad = Writer [String]

parse :: Symbol -> Rules -> Components -> LongestPaths -> [Token] -> ParseMonad Bool
parse initial rules comps lPaths input = do
    let missing = filter (\k -> notMember ('f':k) comps || notMember ('g':k) comps) input
    let hasDollar = "$" `elem` input
    unless (null missing) $ tell ["ERROR: Tokens " ++ show missing ++ " were not defined"]
    when hasDollar $ tell ["ERROR: $ cannot be an input token"]
    if null missing && not hasDollar
    then parse' [] ["$"] (input ++ ["$"])
    else return False
  where
    parse' :: [Symbol] -> [Token] -> [Token] -> ParseMonad Bool
    parse' stack rStack@(lastTk:_) inp@(currTk:rest)
        | lastTk == "$" && currTk == "$" = do
            outputState
            if length stack == 1 && head stack == initial
            then do
                tell ["Action:          Accept phrase"]
                return True
            else do
                tell ["Action:          Reject phrase"]
                return False
        | getF lastTk > getG currTk = do
            outputState
            let cmpGeq = (\tk1 tk2 -> getF tk2 >= getG tk1)
            let sz = length $ takeWhile' cmpGeq rStack
            let rightRule = concat $ reverse $ takeTerminals sz stack
            let leftRule = Map.lookup rightRule rules
            case leftRule of
                Nothing -> do
                    tell ["Phrase " ++ rightRule ++ " cannot be reduced"]
                    tell ["Action:          Reject phrase"]
                    return False
                Just nonTerm -> do
                    tell ["Action:          Reduce with rule " ++ nonTerm ++ " -> " ++ rightRule]
                    parse' (nonTerm : dropTerminals sz stack) (drop sz rStack) inp
        | otherwise = do
            outputState
            tell ["Action:          Shift with " ++ currTk]
            parse' (currTk:stack) (currTk:rStack) rest
      where
        outputState :: ParseMonad ()
        outputState = do
            tell [replicate 30 '-']
            tell ["Stack:           " ++ unwords (reverse stack)]
            tell ["Processed input: " ++ unwords (reverse rStack)]
            tell ["Input:           " ++ unwords inp]
    parse' _ _ _ = do
        tell ["Action:          Reject phrase"]
        return False
    getF :: Token -> Int
    getF tk = fromJust $ getLPath lPaths $ fromJust $ getComponent comps $ 'f':tk
    getG :: Token -> Int
    getG tk = fromJust $ getLPath lPaths $ fromJust $ getComponent comps $ 'g':tk

takeWhile' :: (a -> a -> Bool) -> [a] -> [a]
takeWhile' p (x:y:xs)
    | p x y = x : takeWhile' p (y:xs)
    | otherwise = [x]
takeWhile' _ xs = xs

takeTerminals :: Int -> [Symbol] -> [Symbol]
takeTerminals _ [] = []
takeTerminals n (x:xs)
    | isNonTerminal x   = x : takeTerminals n xs
    | n == 0            = []
    | otherwise         = x : takeTerminals (n-1) xs

dropTerminals :: Int -> [Symbol] -> [Symbol]
dropTerminals _ [] = []
dropTerminals n (x:xs)
    | isNonTerminal x   = dropTerminals n xs
    | n == 0            = x:xs
    | otherwise         = dropTerminals (n-1) xs

isNonTerminal :: Symbol -> Bool
isNonTerminal s = length s == 1 && isUpper (head s)
