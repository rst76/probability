module CoinToss where

import Control.Monad
import Delimited
import Data.Map (empty, insertWith, foldWithKey)


-- メモして高速化

reflect :: PV Bool -> PM Bool
reflect choices = shift (\k -> makeChoices k choices) where
  makeChoices :: (b -> PV b) -> PV b -> PV b
  makeChoices k pv = map f pv where
    f (p, V x) = (p, C $ k x)
    f (p, C x) = (p, C $ makeChoices k x)

flipsXor n = loop n where
  loop 1 = flip_ 0.5
  loop n = do
    y <- loop (n-1)
    x <- flip_ 0.5
    return (x/=y)

flipsXor' n = loop n where
  loop 1 = flip_ 0.5
  loop n = do
    r <- reflect $ explore Nothing $ reify0 $ loop (n-1)
    h <- flip_ 0.5
    return (r/=h)


-- 重み付き選択で高速化

drunkCoin :: PM Bool
drunkCoin = do
  toss <- flip_ 0.5
  lost <- flip_ 0.9
  if lost then dist [] else return toss

dcoinAnd :: Int -> PM Bool
dcoinAnd 0 = return True
dcoinAnd n = do 
  x <- drunkCoin
  if x then dcoinAnd (n-1) else return False


-- 遅延評価で高速化

flipsTrue :: Prob -> Int -> PM Bool
flipsTrue p 0 = return True
flipsTrue p n = do
  x <- flip_ p
  if x then flipsTrue p (n-1) else return False

flips :: Prob -> Int -> PM [Bool]
flips p 0 = return []
flips p n = do
  x <- flip_ p
  xs <- flips p (n-1)
  return (x:xs)

trues :: [Bool] -> PM Bool
trues [] = return True
trues (x:xs) = if x then trues xs else return False

flips_ :: Prob -> Int -> [PM Bool]
flips_ p 0 = []
flips_ p n = flip_ p : flips_ p (n-1)

trues_ :: [PM Bool] -> PM Bool
trues_ [] = return True
trues_ (x : xs) = do
  y <- x
  if y then trues_ xs else return False


-- ユーティリティ

explore :: Ord a => Maybe Int -> PV a -> PV a
explore maxdepth choices = foldWithKey (\v p a -> (p, V v):a) susp ans where
  (ans,susp) = loop 1.0 0 True choices (empty,[])
  loop _ _ _ [] answers = answers
  loop p depth down ((pt,V v):rest) (ans,susp) =
    loop p depth down rest (insertWith (+) v (pt*p) ans, susp)
  loop p depth True ((pt,C t):rest) answers =
    loop p depth True rest $ loop (pt*p) (depth+1) down' t answers where
      down' = case maxdepth of {Just x -> depth<x; Nothing -> True}
  loop p depth down ((pt,c):rest) (ans,susp) =
    loop p depth down rest (ans, (pt*p,c):susp)
