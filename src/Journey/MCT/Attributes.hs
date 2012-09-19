{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Journey.MCT.Attributes (attributes) where

import Data.Maybe (isJust, fromJust, maybeToList)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntervalMap as PM

import Journey.MCT.DecisionTree
import Journey.MCT.Tree
import Journey.MCT.Rule

-- | A container for Int attributes.
type IntStore = IM.IntMap MCTTree

emptyInt :: IntStore
emptyInt = IM.empty

storeInt :: IntStore -> Int -> (MCTTree -> MCTTree) -> IntStore
storeInt s k c = IM.insertWith f k (c Empty) s
  where f _ = c

fetchInt :: IntStore -> Int -> [MCTTree]
fetchInt s k = maybeToList $ IM.lookup k s

-- | A container for Period attributes.
type PeriodStore = PM.IntervalMap Day MCTTree

type Period = (Maybe Day, Maybe Day)

instance Bounded Day where
  minBound = fromGregorian 1900 1 1
  maxBound = fromGregorian 9999 1 1

fromPeriod :: Period -> PM.Interval Day
fromPeriod (a, b) = PM.ClosedInterval x y
  where x = case a of
              Nothing -> minBound
              Just d  -> d
        y = case b of
              Nothing -> maxBound
              Just d  -> d

emptyPeriod :: PeriodStore
emptyPeriod = PM.empty

storePeriod :: PeriodStore -> Period -> (MCTTree -> MCTTree) -> PeriodStore
storePeriod s p c = PM.insertWith f (fromPeriod p) (c Empty) s
  where f _ = c

fetchPeriod :: PeriodStore -> Period -> [MCTTree]
fetchPeriod s p = map snd $ PM.intersecting s (fromPeriod p)

-- | Empty type for instantiation of MCT attribute #1.
data Attr1

instance Attribute MinMCT Attr1 where
  data Store_ MinMCT Attr1 = MkStoreAttr1 IntStore deriving (Show)
  empty_ = MkStoreAttr1 emptyInt
  store_ (MkStoreAttr1 s) (MkMinMCT k) c = MkStoreAttr1
                                         $ storeInt s (fromJust $ rInt k) c
  fetch_ (MkStoreAttr1 s) (MkMinMCT k) = fetchInt s (fromJust $ rInt k)
  exist_ _ = isJust . rInt . getMinMCT

-- | Empty type for instantiation of MCT attribute #2.
data Attr2

instance Attribute MinMCT Attr2 where
  data Store_ MinMCT Attr2 = MkStoreAttr2 PeriodStore deriving (Show)
  empty_ = MkStoreAttr2 emptyPeriod
  store_ (MkStoreAttr2 s) (MkMinMCT k) c = MkStoreAttr2
                                         $ storePeriod s (rDay1 k, rDay2 k) c
  fetch_ (MkStoreAttr2 s) (MkMinMCT k) = fetchPeriod s (rDay1 k, rDay2 k)
  exist_ _ (MkMinMCT (MkRule { rDay1 = a, rDay2 = b })) = isJust a || isJust b

-- | Structure of MCT attributes.
attributes :: [MCTStorable]
attributes = [ MkStorable (empty :: Store MinMCT Attr1)
             , MkStorable (empty :: Store MinMCT Attr2) ]
