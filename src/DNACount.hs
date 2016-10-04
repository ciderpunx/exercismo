module DNACount where
import Data.Map (fromList, Map)

count :: Char -> String -> Either String Int
count x xs
    | notDNA xs   = Left "Shitty DNA"
    | notDNA [x]  = Left "Shitty Nucleotide. I can't search for that"
    | otherwise   = Right . length $ filter (==x) xs

notDNA = any (`notElem` "ACGT")

tidyCount :: Char -> String -> Int
tidyCount x xs = either (const 0) id  (count x xs)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs
    | notDNA xs = Left "Shitty DNA"
    | otherwise = Right . fromList $ map (\n -> (n, tidyCount n xs)) "ACGT"
