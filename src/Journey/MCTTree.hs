{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Journey.MCTTree (
    lookup
  , fromList
  , MCTTree
  , MCT(..)
  , Rank(..)
  ) where

import Data.Monoid (Monoid(..))
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Prelude hiding (lookup)

import qualified Journey.DecisionTree as DT

newtype MCT = MCT { getMCT :: Int }
                  deriving (Eq, Ord, Bounded)

maxMCT :: MCT
maxMCT = MCT maxBound

newtype Rank = Rank { getRank :: Int }
                    deriving (Eq, Ord, Bounded)

minRank :: Rank
minRank = Rank minBound

-- | 
type Item = (MCT, Rank)

newtype MinMCT = MinMCT { getMinMCT :: Item }

instance Monoid MinMCT where
  mempty = MinMCT (maxMCT, undefined)
  (MinMCT (a, x)) `mappend` (MinMCT (b, y)) | a < b     = MinMCT (a, x)
                                            | otherwise = MinMCT (b, y)

newtype MaxRank = MaxRank { getMaxRank :: Item }

instance Monoid MaxRank where
  mempty = MaxRank (undefined, minRank)
  (MaxRank (a, x)) `mappend` (MaxRank (b, y)) | x > y     = MaxRank (a, x)
                                              | otherwise = MaxRank (b, y)

-- | A decision tree of minimum Int values.
type MCTTree = DT.RootTree MinMCT

lookup :: MCTTree -> (DT.Rule, Item) -> MCT -> Maybe Item
lookup t (r, i) k = let i@(_, x) = getMaxRank $ DT.lookupWith l p t (r, MinMCT i)
                    in if x == minBound; then Nothing; else Just i
  where l (MinMCT i)      = MaxRank i
        p (MinMCT (a, _)) = k > a

type Tree = DT.Tree Item

instance DT.StoreAttribute Item Int where
  newtype Store Item Int = MkStoreInt (IM.IntMap Tree)
  empty = MkStoreInt IM.empty
  store d k (MkStoreInt s) c = let k' = getMCT $ fst k
                                   t = IM.findWithDefault d k' s
                               in MkStoreInt $ IM.insert k' (c t) s
  unstore d k (MkStoreInt s) = let k' = getMCT $ fst k
                               in IM.findWithDefault d k' s

instance DT.StoreAttribute Item Double where
  newtype Store Item Double = MkStoreDouble (M.Map Double Tree)
  empty = MkStoreDouble M.empty
  store d k (MkStoreDouble s) c = let k' = fromIntegral . getMCT $ fst k
                                      t = M.findWithDefault d k' s
                                  in MkStoreDouble $ M.insert k' (c t) s
  unstore d k (MkStoreDouble s) = let k' = fromIntegral . getMCT $ fst k
                                  in M.findWithDefault d k' s

s1 = DT.MkStorable (DT.empty :: DT.Store Item Int)
s2 = DT.MkStorable (DT.empty :: DT.Store Item Double)

-- | Create a decision tree from a list of rules and items associations.
fromList :: [(DT.Rule, Item)] -> MCTTree
fromList xs = DT.fromList [] $ map m xs
  where m (r, i) = (r, MinMCT i)

