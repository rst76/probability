module CPS where

import Control.Monad
import Control.Monad.Cont

type Prob = Float
data VC a = V a | C (PV a)
type PV a = [(Prob, VC a)]
type PM a = Cont (PV Bool) a 
type Arr a b = a -> PM b

instance Show a => Show (VC a) where
  show (V x) = "V " ++ show x
  show (C _) = "C <fun>"

pvUnit :: a -> PV a
pvUnit x = [(1.0, V x)]

pvBind :: PV a -> (a -> PV b) -> PV b
pvBind m f = map g m where
  g (p, V x) = (p, C $ f x)
  g (p, C t) = (p, C $ pvBind t f)

b :: a -> PM a
b = return 

dist :: [(Prob, a)] -> PM a
dist ch = Cont $ \k -> map (\(p,v) -> (p, C $ k v)) ch

flip_ :: Prob -> PM Bool
flip_ p = dist [(p, True), (1-p, False)]

neg :: PM Bool -> PM Bool
neg = liftM not

con :: PM Bool -> PM Bool -> PM Bool
con = liftM2 (&&)

dis :: PM Bool -> PM Bool -> PM Bool
dis = liftM2 (||)

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

reify0 :: PM Bool -> PV Bool
reify0 (Cont m) = m pvUnit
