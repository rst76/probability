module CPS0 where

type Prob = Float
data VC a = V a | C (PV a) deriving Show
type PV a = [(Prob, VC a)]
type PM a = (a -> PV Bool) -> PV Bool
type Arr a b = a -> PM b
{-
instance Show a => Show (VC a) where
  show (V x) = "V " ++ show x
  show (C _) = "C <fun>"
-}
unVC :: (Prob, VC a) -> PV a
unVC (p ,C a) = a

b :: Bool -> PM Bool
b x k = k x 

dist :: [(Prob, a)] -> PM a
dist ch k = map (\(p,v) -> (p, C $ k v)) ch

flip_ :: Prob -> PM Bool
flip_ p = dist [(p, True), (1-p, False)]

neg :: PM Bool -> PM Bool
neg e k = e (\ v -> k $ not v)

con :: PM Bool -> PM Bool -> PM Bool
con e1 e2 k = e1 (\v1 -> if v1 then e2 k else b False k)

dis :: PM Bool -> PM Bool -> PM Bool
dis e1 e2 k = e1 (\v1 -> if v1 then b True k else e2 k)

if_ :: PM Bool -> PM Bool -> PM Bool -> PM Bool
if_ et e1 e2 k = et (\t -> if t then e1 k else e2 k)

lam :: (PM a -> PM b) -> PM (Arr a b)
lam e k = k (\x -> e (\k -> k x))

app :: PM (Arr a b) -> PM a -> PM b
app e1 e2 k = e1 (\f -> e2 (\x -> f x k))

let_ :: PM Bool -> (PM Bool -> PM Bool) -> PM Bool
let_ e f = app (lam f) e

reify0 :: PM Bool -> PV Bool
reify0 m = m (\x -> [(1.0, V x)])
