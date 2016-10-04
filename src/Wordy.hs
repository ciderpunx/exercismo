module Wordy where
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>))

data Expr = Number Integer
          | Expr Binop
          | Whatis Expr deriving (Show, Eq)

data Binop = Mult  Expr Expr
           | Div   Expr Expr
           | Plus  Expr Expr
           | Minus Expr Expr deriving (Show, Eq)

evaluate :: Expr -> Expr
evaluate = Number . eval

eval :: Expr -> Integer
eval (Number n)           = n
eval (Expr (Mult e1 e2))  = eval e1 * eval e2
eval (Expr (Div e1 e2))   = eval e1 `div` eval e2
eval (Expr (Plus e1 e2))  = eval e1 + eval e2
eval (Expr (Minus e1 e2)) = eval e1 - eval e2
eval (Whatis e)           = eval e

solve :: String -> Expr
solve eq =
    case evaluate <$> parseLine eq of
      Right a -> a
      Left e  -> error (show e)

parseLine :: String -> Either ParseError Expr
parseLine = parse pExpr "Parse failed"

pExpr :: Parser Expr
pExpr = do
    e <- try pWhatIs <|> try pBinopExp <|> pNum
    spaces
    return e

pNum :: Parser Expr
pNum = do
    sign <- option ' ' (char '-')
    n <- many1 digit
    return (Number (read $ sign : n))

pWhatIs :: Parser Expr
pWhatIs = do
    try (string "What is") <|> string "what is"
    spaces
    e <- pExpr
    return (Whatis e)

pBinopExp :: Parser Expr
pBinopExp = do
    e1 <- pNum  -- <|> pExpr
    spaces
    op <- try (string "multiplied by")
      <|> try (string "times")
      <|> try (string "divided by")
      <|> try (string "plus")
      <|> string "minus"
    spaces
    e2 <- try pExpr <|> pNum
    case op of
      "multiplied by" -> return (Expr (Mult e1 e2))
      "times"         -> return (Expr (Mult e1 e2))
      "divided by"    -> return (Expr (Div e1 e2))
      "plus"          -> return (Expr (Plus e1 e2))
      "minus"         -> return (Expr (Minus e1 e2))

-- an expression is:
-- What is followed by
--  a number optionally followed by                            <-----
--    a binary operator followed by a number optionally followed by _|
