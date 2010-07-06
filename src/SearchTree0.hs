module SearchTree0 where

type Prob = Float
data VC a = V a | C (PV a)
type PV a = [(Prob, VC a)]
type PM a = PV a
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
b = pvUnit

dist :: [(Prob, a)] -> PM a
dist ch = map (\(p,v) -> (p, V v)) ch

flip_ :: Prob -> PM Bool
flip_ p = dist [(p, True), (1-p, False)]

neg :: PM Bool -> PM Bool
neg e = pvBind e (pvUnit . not)

con :: PM Bool -> PM Bool -> PM Bool
con e1 e2 = pvBind e1 (\v1 -> if v1 then e2 else pvUnit False)

dis :: PM Bool -> PM Bool -> PM Bool
dis e1 e2 = pvBind e1 (\v1 -> if v1 then pvUnit True else e2)

if_ :: PM Bool -> PM Bool -> PM Bool -> PM Bool
if_ et e1 e2 = pvBind et (\t -> if t then e1 else e2)

lam :: (PM a -> PM b) -> PM (Arr a b)
lam e = pvUnit (e . pvUnit)

app :: PM (Arr a b) -> PM a -> PM b
app e1 e2 = pvBind e1 (pvBind e2)

let_ :: PM Bool -> (PM Bool -> PM Bool) -> PM Bool
let_ e f = app (lam f) e

reify0 :: PM a -> PV a
reify0 = id
