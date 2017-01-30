module Forth where
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data Err = DivisionByZero
         | StackUnderflow
         | InvalidWord
         | UnknownWord String 
         deriving Show

--type Stack = [Int]

data ForthState = Empty
                | S [Int]
                deriving (Show, Eq)

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
