module Benchmark.benchmark where

import System.CPUTime

measure :: (() -> IO a) -> IO Integer
measure op = do
  nowBefore <- getCPUTime
  x <- (op () >>= (return $!))
  nowAfter <- getCPUTime
  return $! div (nowAfter - nowBefore) 1000


benchmark :: Int -> Int -> (() -> IO a) -> IO Integer
benchmark testNumber n op = do
  bestResult <- fmap minimum . sequence . take n . repeat $ measure op
  putStrLn $ (show testNumber) ++ "," ++ (show bestResult)
  return bestResult


