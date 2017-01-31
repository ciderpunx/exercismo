{-# LANGUAGE OverloadedStrings #-}

module Forth where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T

data Err = DivisionByZero
         | StackUnderflow
         | InvalidWord
         | UnknownWord String 
         deriving Show

data ForthState = Empty
                | S [Int]
                deriving (Show, Eq)

data Exp = RWord T.Text
         | Num Int
         | BinExp BinOp

data BinOp = Plus Exp Exp
           | Minus Exp Exp
           | Div Exp Exp
           | Mul Exp Exp



sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol t = do
    s <- L.symbol sc $ T.unpack t
    return (T.pack s)

integer :: Parser Integer
integer = lexeme L.integer

reservedWord :: T.Text -> Parser ()
reservedWord w = string (T.unpack w) *> notFollowedBy alphaNumChar *> sc

reservedWords :: [T.Text]
reservedWords = ["dup", "drop", "swap", "over"]

-- TODO: type error
--identifier :: Parser T.Text
--identifier =
--    (lexeme . try) (p >>= check)
--  where
--    p       = (:) <$> letterChar <*> many alphaNumChar
--    check x = if x `elem` reservedWords
--              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
--              else return x

toList Empty  = []
toList (S xs) = xs

push :: Int -> ForthState -> Either Err ForthState
push x (S xs) = Right . S $ x : xs

--pop :: Stack -> Either Err (Int, Stack)
--pop [] = Left StackUnderflow
--pop (x:xs) = Right (x, xs)

dup :: ForthState -> Either Err ForthState
dup Empty = Left StackUnderflow
dup (S (x:xs)) = Right . S $ x : x : xs

drp :: ForthState -> Either Err ForthState
drp Empty = Left StackUnderflow
drp (S xs)
    | length xs < 1 = Left StackUnderflow
    | otherwise     = Right . S $ tail xs

swp :: ForthState -> Either Err ForthState
swp Empty = Left StackUnderflow
swp (S xs)
    | length xs < 2 = Left StackUnderflow
    | otherwise     = Right . S $ xs !! 1 : head xs : drop 2 xs

-- TODO: Does OVER mean what I think it means?
ovr :: ForthState -> Either Err ForthState
ovr Empty = Left StackUnderflow
ovr (S xs)
    | length xs < 2 = Left StackUnderflow
    | otherwise     = Right . S $ a : b : a : tl
  where
    (a:b:tl) = xs

-- let xs = [1,2,4]
-- add xs >>= add
-- Right [7]
add :: ForthState -> Either Err ForthState
add Empty = Left StackUnderflow
add (S xs)
    | length xs < 2 = Left StackUnderflow
    | otherwise     = Right . S $ a + b : tl
  where
    (a:b:tl) = xs

-- TODO: other binops
