{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Journey.MCTTree (
    pruneLookup
  , fromList
  , MCTTree
  , MCT(..)
  , undefMCT
  , Rank(..)
  , undefRank
  , Item(..)
  ) where

import Data.Monoid (Monoid(..))
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Prelude hiding (lookup)

import qualified Journey.DecisionTree as DT

newtype MCT = MkMCT { getMCT :: Int }
                    deriving (Eq, Ord, Bounded)

undefMCT :: MCT
undefMCT = MkMCT maxBound

newtype Rank = MkRank { getRank :: Int }
                      deriving (Eq, Ord, Bounded)

undefRank :: Rank
undefRank = MkRank minBound

data Item = MkItem { iMCT :: MCT
                   , iRank :: Rank
                   , iAttr1 :: Maybe Int }
                   deriving (Eq)

newtype MinMCT = MinMCT { getMinMCT :: Item }

instance Monoid MinMCT where
  mempty = MinMCT $ MkItem undefMCT undefined undefined
  mappend i@(MinMCT (MkItem { iMCT = a }))
          j@(MinMCT (MkItem { iMCT = b })) | a < b     = i
                                           | otherwise = j

newtype MaxRank = MaxRank { getMaxRank :: Item }

instance Monoid MaxRank where
  mempty = MaxRank $ MkItem undefined undefRank undefined
  mappend i@(MaxRank (MkItem { iRank = x }))
          j@(MaxRank (MkItem { iRank = y })) | x > y     = i
                                             | otherwise = j

-- | A decision tree of minimum connecting times.
type MCTTree = DT.RootTree MinMCT

-- | The 'pruneLookup' returns the matching element with the highest rank
--   or nothing, discarding branches by minimum MCT.
pruneLookup :: MCTTree -> Item -> Maybe Item
pruneLookup t i = if notfound; then Nothing; else Just result
  where result = getMaxRank $ DT.lookupWith mlift keep t (MinMCT i)
        notfound = iRank result == undefRank
        mlift (MinMCT j) = MaxRank j
        keep (MinMCT (MkItem { iMCT = a })) = (iMCT i) >= a

-- | An intermediate decision tree of minimum connecting times.
type Tree = DT.Tree MinMCT

-- | A container for Int attribues.
type IntStore = IM.IntMap Tree

emptyInt :: IntStore
emptyInt = IM.empty

storeInt :: IntStore -> Int -> (Tree -> Tree) -> IntStore
storeInt s k c = IM.insertWith f k (c DT.Empty) s
  where f _ = c

unstoreInt :: IntStore -> Int -> Tree
unstoreInt s k = IM.findWithDefault DT.Empty k s

existAttr1 :: MinMCT -> Bool
existAttr1 = isJust . iAttr1 . getMinMCT

keyAttr1 :: MinMCT -> Int
keyAttr1 = fromJust . iAttr1 . getMinMCT

data Attr1

instance DT.Attribute MinMCT Attr1 where
  data Store_ MinMCT Attr1 = MkStoreAttr1 IntStore
  empty_ = MkStoreAttr1 emptyInt
  store_ (MkStoreAttr1 s) k c = MkStoreAttr1 $ storeInt s (keyAttr1 k) c
  fetch_ (MkStoreAttr1 s) k = unstoreInt s (keyAttr1 k)
  exist_ _ = existAttr1

s1 = DT.MkStorable (DT.empty :: DT.Store MinMCT Attr1)

{-
instance DT.StoreAttribute Item Double where
  newtype Store Item Double = MkStoreDouble (M.Map Double Tree)
  empty = MkStoreDouble M.empty
  store d k (MkStoreDouble s) c = let k' = fromIntegral . getMCT $ fst k
                                      t = M.findWithDefault d k' s
                                  in MkStoreDouble $ M.insert k' (c t) s
  unstore d k (MkStoreDouble s) = let k' = fromIntegral . getMCT $ fst k
                                  in M.findWithDefault d k' s


s2 = DT.MkStorable (DT.empty :: DT.Store Item Double)
-}

-- | Create a decision tree from a list of rules and items associations.
fromList :: [Item] -> MCTTree
fromList xs = DT.fromList [s1] $ map MinMCT xs
