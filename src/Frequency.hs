{-# LANGUAGE OverloadedStrings #-}

module Frequency (frequency) where

import Control.Arrow     ((&&&))
import Control.DeepSeq   (force)
import Control.Parallel.Strategies (runEval, rpar, rseq)
import Data.Char         (isLetter)
import Data.Map          (fromListWith, Map)
import Data.List         (sort,group)
import Data.List.Split   (chunksOf)
import Prelude    hiding (lookup)
import qualified Data.Text as T

frequency :: Int -> [T.Text] -> Map Char Integer
frequency numThreads xss =
    if numThreads < 1 || T.length xs > 0 && numThreads > T.length xs
    then error "Bad number of threads"
    else
      runEval $ do
        rs <- mapM (rpar . force frq) xs' -- creates threads 1 per chunk of list
        mapM_ rseq rs                     -- waits for threads to complete 
        return . fromListWith (+) $ concat rs
  where
      xs  = T.concat xss
      xs' = chunksOf (T.length xs `div` numThreads)
          . filter isLetter
          . T.unpack
          $ T.toLower xs

frq :: String -> [(Char, Integer)]
frq xs =
    map (head &&& toInteger . length) . group $ sort xs
