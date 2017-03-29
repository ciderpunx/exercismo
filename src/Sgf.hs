{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- Good docs for SGF format at http://www.red-bean.com/sgf/sgf4.html
module Sgf(parseSgf) where

import qualified Data.Text as T -- hiding (zipWith,concat, map)
import qualified Data.Map as M
import Data.Tree
import Text.Parsec hiding (Empty)
import Text.Parsec.Char

type Property = (T.Text, [T.Text])
type GameTree = [SgfTree]
type SgfTree = Tree SgfNode
type SgfNode = M.Map T.Text [T.Text]

parseSgf :: T.Text -> Maybe SgfTree
parseSgf xs =
    case parse gameTree "" xs of
      Left err -> Nothing
      Right tree -> Just tree

sgfTreeToList :: SgfTree -> [[Property]]
sgfTreeToList (Node lbl subForest) =
      M.toList lbl : concatMap sgfTreeToList subForest

-- A gametree is a ( followed by zero or more sequences followed by 1 or more gametrees followed by )
gameTree  :: Parsec T.Text () SgfTree
gameTree = do
    _ <- char '('
    s <- sequ
    _ <- char ')'
    return s

-- Sequence (sequ) is paren followed by 1 or more nodes followed by 0 or more sub-sequences or gameTrees followed by bracket
-- This doesn't exactly follow the spec -- game trees ought to come after subsequences, but its slightly easier to write this way
sequ  :: Parsec T.Text () SgfTree
sequ = do
  n <- node
  ns <- many (gameTree <|> sequ)
  return $ Node (M.fromList n) ns

-- Node is a semicolon followed by 1 or more properties
node :: Parsec T.Text () [Property]
node = do
    _ <- char ';'
    many1 property

-- Property is propIdent followed by 1 or more propVals.
property :: Parsec T.Text () Property
property = do
    key <- propIdent
    items <- many1 propVal
    return (key, items)

-- The spec only allows for propIdents to have 1 or 2 capital letters we consume more but
-- return just the first 2
propIdent :: Parsec T.Text () T.Text
propIdent = do
    a <- upper
    b <- many upper
    return $ T.pack (a:b)

-- propVal is square bracket followed by 0 or more letters (for our purposes) 
-- followed by square bracket
propVal :: Parsec T.Text () T.Text
propVal = do
  _ <- char '['
  val <- many' propChars
  _ <- char ']'
  return $ cleanWhitespace val -- (filter (\v -> v/='\r' && v/='\n') val)

--propChars :: ParsecT T.Text u Data.Functor.Identity.Identity Char
propChars = escaped <|> noneOf "]"

-- dealing wth escaped chars, code from https://codereview.stackexchange.com/questions/2406/parsing-strings-with-escaped-characters-using-parsec
escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
escapedChar code replacement = char code >> return replacement
codes        = "bnfrt\\\"/[]()\r\n"
replacements = "\b\n\f\r\t\\\"/[]()\0\0"

-- convenience, parse many chars then pack into a text
many' :: Parsec T.Text () Char -> Parsec T.Text () T.Text
many' = fmap T.pack . many

cleanWhitespace :: T.Text -> T.Text
cleanWhitespace =
      T.replace "\0" ""
    . T.replace "\t" " "
    . T.replace "\r" " "
    . T.replace "\n" " "
    . T.replace "\r\n" " "
