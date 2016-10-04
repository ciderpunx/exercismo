module Wordy2 where
import Text.ParserCombinators.Parsec

-- In reality you'd use an Either here so you could cpature the error
answer :: String -> Maybe Integer
answer eq =
    case parse parseLine "FAIL" eq of
      Right a -> Just a
      Left e  -> Nothing -- error (show e)

-- Take a line of input, and parse into a what is token, at least 1 number and 0 or more
-- operation/number pairs to perform. Then perform the ops and return the result.
-- the important trick is that we simply compose any operations together, along with 
-- the id for the first number
parseLine :: Parser Integer
parseLine = do
    spaces
    _ <- pWhatIs
    n0 <- pNum
    opNs <- many pOpN
    return $ compose opNs n0
  where
    compose = foldr (flip (.)) id

-- An operation followed by a number (an expression may have 0 or more occurences)
-- returns a function for the represented operation which can be composed in parseLine
pOpN :: Parser (Integer -> Integer)
pOpN  = do
   spaces
   op <- try (string "multiplied by")
     <|> try (string "times")
     <|> try (string "divided by")
     <|> try (string "plus")
     <|> string "minus"
   spaces
   n <- pNum
   case op of
      "multiplied by" -> return (*n)
      "times"         -> return (*n)
      "divided by"    -> return (`div`n)
      "plus"          -> return (+n)
      "minus"         -> return (\x -> x-n)

-- Just a number -- there should always be at least 1 number
pNum :: Parser Integer
pNum = do
    sign <- option ' ' (char '-')
    n <- many1 digit
    return (read $ sign : n)

-- The string "What is" or "what is" if you don't  capitalize.
pWhatIs :: Parser ()
pWhatIs = do
    try (string "What is") <|> string "what is"
    spaces
    return ()
