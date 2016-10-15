module Etl where
import Data.Char (toLower)
import Data.Map (Map, assocs, fromList)

transform :: Map Int String -> Map Char Int
transform = fromList . concatMap (\(k,vs) -> zip (map toLower vs) (repeat k)) . assocs
