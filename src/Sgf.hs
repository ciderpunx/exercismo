{-# LANGUAGE OverloadedStrings #-}

module Sgf(parseSgf) where

import Data.Maybe (fromJust)
import Data.Tree
import Text.Parsec hiding (Empty)
import qualified Data.Text as T
import qualified Data.Map as M

type Property = (T.Text, [T.Text])
type GameTree = [SgfTree]
type SgfTree = Tree SgfNode
type SgfNode = M.Map T.Text [T.Text]

parseSgf :: T.Text -> Maybe SgfTree
parseSgf xs =
    case parse gameTree "" xs of
      Left err -> Nothing
      Right tree -> Just tree

-- A gameTree is ( followed by 0 or more sequences followed by 1 or more gameTrees followed by )
gameTree  :: Parsec T.Text () SgfTree
gameTree = do
    _ <- char '('
    s <- sequ
    ts <- many gameTree
    _ <- char ')'
    return (makeTree s ts)

makeTree :: [SgfNode] -> [SgfTree] -> SgfTree
makeTree [] _      = error "Shouldn't happen, maybe something wrong in Parsec library many1 function?"
makeTree [n] ts    = Node n ts
makeTree (n:ns) ts = Node n [makeTree ns ts]

-- A sequence (sequ) is 1 or more nodes
sequ :: Parsec T.Text () [SgfNode]
sequ = return . map M.fromList =<< many1 node

-- A node is a semicolon followed by 0 or more properties
node :: Parsec T.Text () [Property]
node = char ';' *> many property

-- A property is propIdent followed by 1 or more propVals.
property :: Parsec T.Text () Property
property = do
    key <- propIdent
    items <- many1 propVal
    return (key, items)

-- A propIdent is 1 or 2 capital letters
propIdent :: Parsec T.Text () T.Text
propIdent = do
    a <- upper
    b <- optUpper
    return $ T.pack (a:b)
  where
    optUpper = option "" (do {u <- upper; return [u] })

-- A propVal is square bracket followed by 0 or more propChars (for our purposes) followed by square bracket
propVal :: Parsec T.Text () T.Text
propVal = return . cleanWhitespace =<< between (char '[') (char ']') (many' propChars)

-- A propChar is either an escaped character or any other character except ]
propChars :: Parsec T.Text () Char
propChars = escaped <|> noneOf "]"

-- The next few functions deal with escaped chars
-- code from: https://codereview.stackexchange.com/questions/2406/parsing-strings-with-escaped-characters-using-parsec
escaped :: Parsec T.Text () Char
escaped = char '\\' >> choice (zipWith escapedChar codes replacements)

escapedChar :: Char -> Char -> Parsec T.Text () Char
escapedChar code replacement = char code >> return replacement

codes :: String
codes = "bnfrt\\\"/[]()\r\n"

replacements :: String
replacements = "\b\n\f\r\t\\\"/[]()\0\0"

-- Parse many chars then pack into a Data.Text
many' :: Parsec T.Text () Char -> Parsec T.Text () T.Text
many' = fmap T.pack . many

-- Squish newlines and tabs into single spaces, remove nuls
cleanWhitespace :: T.Text -> T.Text
cleanWhitespace =
      T.replace "\0" ""
    . T.replace "\t" " "
    . T.replace "\r" " "
    . T.replace "\n" " "
    . T.replace "\r\n" " "
