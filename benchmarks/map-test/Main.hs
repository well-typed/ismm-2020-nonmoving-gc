{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Main(main) where

import qualified Data.Map.Strict as M
import System.Random.SplitMix
import System.Random
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import BenchUtils

data Op k v
  = Insert k v
  | Delete k
  | Lookup k
  deriving (Show)

newtype Rand a = Rand { getRand :: State SMGen a }
  deriving (Functor, Applicative, Monad)

liftRand :: (SMGen -> (a, SMGen)) -> Rand a
liftRand = Rand . state

randInt :: Rand Int
randInt = liftRand nextInt

random' :: Random a => Rand a
random' = liftRand random

runRand :: SMGen -> Rand a -> a
runRand g (Rand m) = evalState m g

randomOp :: (Random k, Random v) => Rand (Op k v)
randomOp = do
  tag <- (`mod` 3) <$> randInt
  case tag of
    0 -> Insert <$> random' <*> random'
    1 -> Delete <$> random'
    2 -> Lookup <$> random'

randoms' :: SMGen -> Rand a -> [a]
randoms' g0 (Rand m) = go g0
  where
    go g = let (x, g') = runState m g
            in x : go g'

interpretOp :: (Ord k, Monad m)
            => Op k v -> StateT (M.Map k v) m ()
interpretOp (Insert k v) = modify $ M.insert k v
interpretOp (Delete k) = modify $ M.delete k
interpretOp (Lookup k) = do
  !_ <- M.lookup k <$> get
  return ()

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = case splitAt n xs of
              (as, bs) -> as : go bs

main :: IO ()
main = do
  let gen = mkSMGen 42
  let ops = randoms' gen randomOp :: [Op Int Int]
  let n = 20000000
  let chunks :: [[Op Int Int]]
      chunks = chunksOf 100000 $ take n ops

  let doChunk :: Ord k => [Op k v] -> StateT (M.Map k v) IO ()
      doChunk chunk = timeIt "chunk" $ do
        mapM_ interpretOp chunk
        !_ <- get
        return ()
        
  putStrLn "hi1a"
  res <- evalStateT (mapM_ doChunk chunks >> fmap sum get) mempty
  print res

