module Journey.TypeNum where

data Zero
data Succ a

class Number a where
  numValue :: a -> Int

instance Number Zero where
  numValue = const 0
instance Number x => Number (Succ x) where
  numValue x = numValue (numPred x) + 1

numPred :: Succ a -> a
numPred = const undefined

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

