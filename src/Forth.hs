{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Forth (ForthError(..), empty, eval, evalText, toList, runStr) where

import Control.Monad (foldM)
import Text.Parsec hiding (Empty)
import Prelude hiding (subtract)
import qualified Data.Text as T
import Data.Char (toLower, isAlphaNum)

data ForthError = DivisionByZero
                | StackUnderflow
                | InvalidWord
                | UnknownWord T.Text
                deriving (Show, Eq)

data ForthState = S ([(Token,[Token])], [Int]) 
                deriving Show

data Token = BinOp Op
           | RWord T.Text
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
-- I use strings here so that I can take advantage of the letter and digit parsers
-- I should really write my own parsers so that the commented out type signatures
-- are what we use.

--punctuation :: T.Text
punctuation :: String
punctuation = "-_?!"

--defPunc :: T.Text
defPunc :: String
defPunc = ";:"

--operators :: T.Text
operators :: String
operators = "+-/*"

-- will be needed if I write my own digit and letter parsers
--telem :: Char -> T.Text -> Bool
--telem c t = T.pack [c] `T.isInfixOf` t

isAlphaNumOrPuncOrOperator :: Char -> Bool
isAlphaNumOrPuncOrOperator c = isAlphaNum c || c `elem` defPunc || c `elem` punctuation || c `elem` operators

nonAlphaNum :: (Stream s m Char) => ParsecT s u m Char
nonAlphaNum = satisfy (not . isAlphaNumOrPuncOrOperator) <?> "nonAlphaNum"

whiteSpace :: (Stream s m Char) => ParsecT s u m ()
whiteSpace = skipMany (space <|> nonAlphaNum) <?> "whitespace"

progP :: Parsec T.Text () [Token]
progP = do
    tokens <- tokeP `sepEndBy` whiteSpace
    _ <- eof
    return tokens

tokeP :: Parsec T.Text () Token
tokeP =
    binOpP <|> defP <|> wordP <|> numP

binOpP :: Parsec T.Text () Token
binOpP  = do
      n <- oneOf "+-/*"
      case n of
        '+' -> return (BinOp Plus)
        '-' -> return (BinOp Minus)
        '/' -> return (BinOp Div)
        '*' -> return (BinOp Mul)

wordP :: Parsec T.Text () Token
wordP = fmap RWord $ do
            x <- letter
            xs <- many (letter <|> digit <|> puncP)
            return $ T.pack (x:xs)

puncP :: Parsec T.Text () Char
puncP = oneOf punctuation

numP :: Parsec T.Text () Token
numP = fmap Num intP

intP :: Parsec T.Text () Int
intP = do
  s <- signP
  ds <- many1 digit
  return . s $ read ds

signP :: Parsec T.Text () (Int -> Int)
signP = (char '-' >> return negate) <|> (optional (char '+') >> return id)

defP :: Parsec T.Text () Token
defP = do
        _ <- char ':'
        whiteSpace
        w <- tokeP
        whiteSpace
        ws <- tokeP `sepEndBy` whiteSpace
        _ <- char ';'
        return (Def w ws)

-- Interpreting
runStr :: T.Text -> Either ForthError [Int]
runStr = fmap toList . eval

runTxts :: [T.Text] -> Either ForthError [Int]
runTxts = fmap toList . foldM (flip evalText) empty

eval :: T.Text -> Either ForthError ForthState
eval t = evalText t empty

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText t state =
    let ts = parse progP "" (T.toLower t)
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

add, subtract, multiply, divide :: ForthState -> Either ForthError ForthState
add      = binOp (+)
subtract = binOp (-)
multiply = binOp (*)
divide s@(S (e,xs))
  | length xs < 2 = Left StackUnderflow
  | head xs == 0  = Left DivisionByZero
  | otherwise     = binOp div s
