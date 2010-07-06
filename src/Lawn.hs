module Lawn where

import SearchTree
-- import CPS
-- import Delimited
import Data.Map (empty, insertWith, foldWithKey)

grassModel :: PM Bool
grassModel =
  let_ (flip_ 0.3) (\ rain ->
  let_ (flip_ 0.5) (\ sprinkler ->
  let_ (dis (con (flip_ 0.9) rain)
            (dis (con (flip_ 0.8) sprinkler)
                 (flip_ 0.1))) (\ grassIsWet ->
  if_ grassIsWet rain (dist []))))

-- モナドバージョンのモジュールを呼び出すときは、以下の定義が使える。
{-
grassModel = do
  rain           <- flip_ 0.3
  sprinkler      <- flip_ 0.5
  wetInRain      <- flip_ 0.9
  wetInSprinkler <- flip_ 0.8
  wetInOther     <- flip_ 0.1
  let grassIsWet = rain && wetInRain
                || sprinkler && wetInSprinkler
                || wetInOther
  if grassIsWet then return rain else dist []
-}

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
