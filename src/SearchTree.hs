module SearchTree where

import Control.Monad

type Prob = Float
data VC a = V a | C (PV a)
type PV a = [(Prob, VC a)]
newtype PM a = M {unM :: PV a}
type Arr a b = a -> PM b

instance Show a => Show (VC a) where
  show (V x) = "V " ++ show x
  show (C _) = "C <fun>"

instance Show a => Show (PM a) where
  show (M x) = show x

instance Monad PM where
  return x = M [(1.0, V x)]
  M m >>= f = M $ map g m where
    g (p, V x) = (p, C $ unM $ f x)
    g (p, C t) = (p, C $ unM $ M t >>= f)

b :: a -> PM a
b = return

dist :: [(Prob, a)] -> PM a
dist ch = M $ map (\(p,v) -> (p, V v)) ch

flip_ :: Prob -> PM Bool
flip_ p = dist [(p, True), (1-p, False)]

neg :: PM Bool -> PM Bool
neg e = do
  x <- e
  return (not x)
-- neg = liftM not

con :: PM Bool -> PM Bool -> PM Bool
con e1 e2 = do
  v1 <- e1
  if v1 then e2 else return False
-- con = liftM2 (&&)

dis :: PM Bool -> PM Bool -> PM Bool
dis e1 e2 = do
  v1 <- e1
  if v1 then return True else e2
-- dis = liftM2 (||)

if_ :: PM Bool -> PM Bool -> PM Bool -> PM Bool
if_ et e1 e2 = do
  t <- et
  if t then e1 else e2

lam :: (PM a -> PM b) -> PM (Arr a b)
lam e = return (e . return)

app :: PM (Arr a b) -> PM a -> PM b
app e1 e2 = do
  f <- e1
  x <- e2
  f x

let_ :: PM Bool -> (PM Bool -> PM Bool) -> PM Bool
let_ e f = app (lam f) e

reify0 :: PM a -> PV a
reify0 = unM
