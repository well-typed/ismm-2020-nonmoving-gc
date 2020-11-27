{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment

main :: IO ()
main = do
  [s] <- getArgs
  let n = read s :: Int
      xs :: [Int]
      !xs = [0..n]
  
  length xs `seq` print (length xs)
