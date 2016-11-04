module Pascal where

rows :: Int -> [[Integer]]
rows n
    | n < 1     = []
    | otherwise = take n $ iterate getRow [1]

getRow :: [Integer] -> [Integer]
getRow [] = []
getRow r  =
    let r' = 0 : reverse (0 : r) -- or 0 : r ++ [0]
    in zipWith (+) r' (tail r')

getRow' :: [Integer] -> [Integer]
getRow' []   = [1]
getRow' [1]  = [1,1]
getRow' prev =
    1 : map (\x -> prev !! x + prev !! (x+1)) [0..length prev - 2] ++ [1]

displayRows :: Int -> IO ()
displayRows n =
    if n > 15
    then putStr . unlines . map show $ rows n
    else let rs      = rows n
             numRows = length rs
             longest = length . show $ last rs
         in putStr $ concatMap (\x -> fmt x (rs !! x) longest) [0..numRows-1]
  where
    fmt x row l = (concat . take (l`div`2 - x) $ repeat " ") ++ show row ++ "\n"
