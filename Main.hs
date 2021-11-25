{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Pipeline
import Prelude hiding (id, (<>))
import GHC.Float (int2Double)
import Data.Foldable (maximumBy)
import System.Environment (getArgs)
import Data.List (sort)
import Pipeline.Internal.Common.Nat (IsNat(nat))

meanTime :: Circuit '[CSVStore] '[[(String, Double)]] '[Var] '[Double] N1
meanTime = functionTask f
  where
    f xs = let times = map snd xs in (sum times :: Double) / int2Double (length times)

medianTime :: Circuit '[CSVStore] '[[(String, Double)]] '[Var] '[Double] N1
medianTime = functionTask f
  where
    f xs = sort (map snd xs) !! (length xs `div` 2)

longestTime :: Circuit '[CSVStore] '[[(String, Double)]] '[Var] '[(String, Double)] N1
longestTime = functionTask f
  where
    f xs = maximumBy (\(_, time1) (_, time2) -> compare time1 time2) xs

pipeline :: Circuit '[CSVStore] '[[(String, Double)]] '[Var, Var, Var] '[Double, Double, (String, Double)] N1
pipeline =
    replicateN (nat @ N3)
    <-> meanTime
    <>  medianTime
    <>  longestTime

main :: IO ()
main = do
    inputPath <- fmap head getArgs
    n <- startNetwork pipeline

    input_ (HCons' (CSVStore inputPath) HNil') (n :: BasicNetwork '[CSVStore] '[[(String, Double)]] '[Var, Var, Var] '[Double, Double, (String, Double)])
    (Right (HCons' meanStore (HCons' medianStore (HCons' longestNameStore HNil')))) <- output_ n

    mean <- fetch meanStore
    median <- fetch medianStore
    longestName <- fetch longestNameStore

    print mean
    print median
    print longestName
