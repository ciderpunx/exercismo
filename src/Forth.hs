{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Forth (ForthError(..), empty, eval, evalText, toList, runStr) where

import Control.Monad (foldM)
import Text.Parsec hiding (Empty)
import Prelude hiding (subtract)
import Text.ParserCombinators.Parsec.Number
import qualified Data.Text as T
import Data.Char (toLower, isAlphaNum)

data ForthError = DivisionByZero
                | StackUnderflow
                | InvalidWord
                | UnknownWord String
                deriving (Show, Eq)

data ForthState = S ([(Token,[Token])], [Int]) 
                deriving Show

data Token = BinOp Op
           | RWord String
           | Def Token [Token]
           | Num Int
           deriving (Show, Eq)

data Op = Plus
        | Minus
        | Mul
        | Div
        deriving (Show, Eq)

empty :: ForthState
empty = S ([],[])

toList :: ForthState -> [Int]
toList (S (_,stack)) = reverse stack

-- Parsing
-- TODO: the exercise really wants us to process Text not String.

punctuation :: String
punctuation = "-_?!"

defPunc :: String
defPunc = ";:"

operators :: String
operators = "+-/*"

isAlphaNumOrPuncOrOperator :: Char -> Bool
isAlphaNumOrPuncOrOperator c = isAlphaNum c || c `elem` defPunc || c `elem` punctuation || c `elem` operators

nonAlphaNum :: (Stream s m Char) => ParsecT s u m Char
nonAlphaNum = satisfy (not . isAlphaNumOrPuncOrOperator) <?> "nonAlphaNum"

whiteSpace :: (Stream s m Char) => ParsecT s u m ()
whiteSpace = skipMany (space <|> nonAlphaNum) <?> "whitespace"

progP :: Parsec String () [Token]
progP = do
    tokens <- tokeP `sepEndBy` whiteSpace
    _ <- eof
    return tokens

tokeP :: Parsec String () Token
tokeP =
    binOpP <|> defP <|> wordP <|> numP
  where
    binOpP  = do
      n <- oneOf "+-/*"
      case n of
        '+' -> return (BinOp Plus)
        '-' -> return (BinOp Minus)
        '/' -> return (BinOp Div)
        '*' -> return (BinOp Mul)
    wordP = fmap RWord $ do
            x <- letter
            xs <- many (letter <|> digit <|> puncP)
            return (x:xs)
    puncP = oneOf punctuation
    numP = fmap Num int
    defP = do
        _ <- char ':'
        whiteSpace
        w <- tokeP
        whiteSpace
        ws <- tokeP `sepEndBy` whiteSpace
        _ <- char ';'
        return (Def w ws)

-- Interpreting
runStr :: String -> Either ForthError [Int]
runStr = fmap toList . eval

-- this is kind of dumb because we already deal with folding through the
-- input (in evalStr)
runTxts :: [T.Text] -> Either ForthError [Int]
runTxts = fmap toList . foldM (flip evalText) empty

eval :: String -> Either ForthError ForthState
eval str = evalStr str empty

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText = evalStr . T.unpack

evalStr :: String -> ForthState -> Either ForthError ForthState
evalStr str state =
    let ts = parse progP "" (map toLower str)
    in case ts of
      Left p -> error $ "Parse error: " ++ show p
      Right tokens -> evalToks state tokens

evalToks :: ForthState -> [Token] -> Either ForthError ForthState
evalToks = foldM evalTok

evalTok :: ForthState -> Token -> Either ForthError ForthState
evalTok (S (env, stack)) (Num x) = Right $ S (env, x:stack)
evalTok state (BinOp Plus)       = add state
evalTok state (BinOp Minus)      = subtract state
evalTok state (BinOp Mul)        = multiply state
evalTok state (BinOp Div)        = divide state
evalTok (S (env, stack)) (Def k ts) =
    case k of
      (RWord x) -> Right (S ((k,ts) : env, stack))
      otherwise ->  Left InvalidWord -- this was originally a parser error, but tests fail that way
evalTok state@(S (env, _)) t@(RWord w) =
    case lookup t env of
      Just tokens -> evalToks state tokens -- NB repetition, TODO
      Nothing -> case w of
        "dup"     -> dup state
        "drop"    -> drp state
        "swap"    -> swp state
        "over"    -> ovr state
        otherwise ->  Left (UnknownWord w)

dup :: ForthState -> Either ForthError ForthState
dup (S (e,xs))
    | length xs < 1 = Left StackUnderflow
    | otherwise     = Right $ S (e, x : xs)
  where
    x = head xs

drp :: ForthState -> Either ForthError ForthState
drp (S (e,xs))
    | length xs < 1 = Left StackUnderflow
    | otherwise     = Right $ S (e, tail xs)

swp :: ForthState -> Either ForthError ForthState
swp (S (e,xs))
    | length xs < 2 = Left StackUnderflow
    | otherwise     = Right $ S (e, xs !! 1 : head xs : drop 2 xs)

ovr :: ForthState -> Either ForthError ForthState
ovr (S (e,xs))
    | length xs < 2 = Left StackUnderflow
    | otherwise     = Right $ S (e, b : a : b : tl)
  where
    (a:b:tl) = xs

binOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binOp o (S (e,xs))
    | length xs < 2 = Left StackUnderflow
    | otherwise     = Right $ S (e, o b a : tl)
  where
    (a:b:tl) = xs

add      = binOp (+)
subtract = binOp (-)
multiply = binOp (*)
divide s@(S (e,xs))
  | length xs < 2 = Left StackUnderflow
  | head xs == 0  = Left DivisionByZero
  | otherwise     = binOp div s
