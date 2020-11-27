{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import CAR.Types
import qualified CAR.TocFile as Toc

main :: IO ()
main = do
  [f] <- getArgs
  Toc.createIndex pageName f
  return ()
