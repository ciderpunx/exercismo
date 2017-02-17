{-# LANGUAGE OverloadedStrings #-}

module Forth where

import Control.Monad (foldM)
import Text.Parsec hiding (Empty)
import Prelude hiding (subtract)
import Text.ParserCombinators.Parsec.Number

data ForthError = DivisionByZero
         | StackUnderflow
         | InvalidWord
         | UnknownWord String
         deriving Show

data ForthState = S ([(String,Token)], [Int]) deriving Show

data Token = BinOp Op
           | RWord String
           | Def Token [Token]
           | Num Int
           deriving Show

data Op = Plus
        | Minus
        | Mul
        | Div
        deriving Show

eval str =
    let ts = parse progP "" str
    in case ts of
      Left p -> error $ "Parse error: " ++ show p
      Right tokens -> foldM evalTok empty tokens

evalTok :: ForthState -> Token -> Either ForthError ForthState
evalTok (S (env, ss)) (Num x) = Right $ S (env, x:ss)
evalTok ss (BinOp Plus)       = add ss
evalTok ss (BinOp Minus)      = subtract ss
evalTok ss (BinOp Mul)        = multiply ss
evalTok ss (BinOp Div)        = divide ss
evalTok ss (RWord "dup")      = dup ss
evalTok ss (RWord "drop")     = drp ss
evalTok ss (RWord "swap")     = swp ss
-- evalTok (S (env, ss)) (RWord w) = undefined -- see if word is in the env
evalTok ss (RWord xs)         = Left $ UnknownWord xs



empty = S ([],[])

progP :: Parsec String () [Token]
progP = do
    tokens <- tokeP `sepEndBy` spaces
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
            xs <- many (letter <|> digit)
            return (x:xs)
    numP = fmap Num int
    defP = do
        _ <- char ':'
        spaces
        w <- wordP
        spaces
        ws <- tokeP `sepEndBy` spaces
        _ <- char ';'
        return (Def w ws)

toList (S (_,xs)) = xs

push :: Int -> ForthState -> Either ForthError ForthState
push x (S (e,xs)) = Right $ S (e, x : xs)

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

-- TODO: Does OVER mean what I think it means?
ovr :: ForthState -> Either ForthError ForthState
ovr (S (e,xs))
    | length xs < 2 = Left StackUnderflow
    | otherwise     = Right $ S (e, a : b : a : tl)
  where
    (a:b:tl) = xs

binOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binOp o (S (e,xs))
    | length xs < 2 = Left StackUnderflow
    | otherwise     = Right $ S (e, o b a : tl)
  where
    (a:b:tl) = xs

-- let xs = [1,2,4]
-- add xs >>= add
-- Right [7]
add      = binOp (+)
subtract = binOp (-)
multiply = binOp (*)
divide s@(S (e,xs))
  | length xs < 2 = Left StackUnderflow
  | xs !! 1 == 0  = Left DivisionByZero
  | otherwise     = binOp div s
