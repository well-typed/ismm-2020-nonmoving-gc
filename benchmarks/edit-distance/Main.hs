{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.MemoTrie
import System.Environment
import BenchUtils

main :: IO ()
main = do
  [n, inputFile] <- getArgs
  inputs <- lines <$> readFile inputFile
  let n' = read n
  sequence_
    [ timeIt "req" $ do
        let dist = sed (take n' in1) (take n' in2)
        print (i1, i2, dist)
    | (i1, in1) <- zip [0..] inputs
    , (i2, in2) <- zip [0..] inputs
    ]

sed :: String -> String -> Int
sed = memo2 go
  where
    go "" bs =
      length bs
    go as "" =
      length as
    go (a:as) (b:bs) =
      let m = (if a == b then 0 else 1) + sed as bs
          i = 1 + sed (a:as) bs
          d = 1 + sed as (b:bs)  
      in m `min` i `min` d

