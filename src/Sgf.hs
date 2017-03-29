{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Sgf(parseSgf) where

import Data.Text hiding (zipWith)
import Text.Parsec hiding (Empty)
import Text.Parsec.Char

type NodeDatum = (Text, [Text])
data Node = Node [NodeDatum] [Node] deriving Show
data Statement = Stmt [Node] [Statement] deriving Show

parseSgf :: Text -> Maybe Statement
parseSgf xs =
    case parse statement "" xs of
      Left err -> Nothing
      Right stmt -> Just stmt

-- statement is paren followed by 0 or more nodes followed by 0 or more statements folowed by bracket
statement :: Parsec Text () Statement
statement = do
    _ <- char '('
    nodes <- many node
    stmts <- many statement
    _ <- char ')'
    return $ Stmt nodes stmts

-- node is semicolon followed by capital letter followed by 1 or more items
node :: Parsec Text () Node
node = do
    _ <- char ';'
    key <- many1' upper
    items <- many1 item
    let nd =(key, items) :: NodeDatum
    return (Node [nd] [])

-- item is square bracket followed by 1 or more letters followed by square bracket
item :: Parsec Text () Text
item = do
  _ <- char '['
  val <- many' itemChars
  _ <- char ']'
  return $ cleanWhitespace val -- (filter (\v -> v/='\r' && v/='\n') val)

itemChars = escaped <|> noneOf "]"

-- dealing wth escaped chars, code from https://codereview.stackexchange.com/questions/2406/parsing-strings-with-escaped-characters-using-parsec
escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
escapedChar code replacement = char code >> return replacement
codes        = "bnfrt\\\"/[]()\r\n"
replacements = "\b\n\f\r\t\\\"/[]()\0\0"

-- convenience, parse many chars then pack into a text
many' :: Parsec Text () Char -> Parsec Text () Text
many' = fmap pack . many

--many1' :: ParsecT Text u Data.Functor.Identity.Identity Char -> ParsecT Text u Data.Functor.Identity.Identity Text
many1' = fmap pack . many1

cleanWhitespace :: Text -> Text
cleanWhitespace =
      replace "\0" ""
    . replace "\t" " "
    . replace "\r" " "
    . replace "\n" " "
    . replace "\r\n" " "
