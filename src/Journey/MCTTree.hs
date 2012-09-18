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
  , Rule(..)
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

data Rule = MkRule { iMCT :: MCT
                   , iRank :: Rank
                   , iAttr1 :: Maybe Int }
                   deriving (Eq)

newtype MinMCT = MinMCT { getMinMCT :: Rule }

instance Monoid MinMCT where
  mempty = MinMCT $ MkRule undefMCT undefined undefined
  mappend i@(MinMCT (MkRule { iMCT = a }))
          j@(MinMCT (MkRule { iMCT = b })) | a < b     = i
                                           | otherwise = j

newtype MaxRank = MaxRank { getMaxRank :: Rule }

instance Monoid MaxRank where
  mempty = MaxRank $ MkRule undefined undefRank undefined
  mappend i@(MaxRank (MkRule { iRank = x }))
          j@(MaxRank (MkRule { iRank = y })) | x > y     = i
                                             | otherwise = j

-- | A decision tree of minimum connecting times.
type MCTTree = DT.RootTree MinMCT

-- | The 'pruneLookup' returns the matching element with the highest rank
--   or nothing, discarding branches by minimum MCT.
pruneLookup :: MCTTree -> Rule -> Maybe Rule
pruneLookup t i = if notfound; then Nothing; else Just result
  where result = getMaxRank $ DT.lookupWith mlift keep t (MinMCT i)
        notfound = iRank result == undefRank
        mlift (MinMCT j) = MaxRank j
        keep (MinMCT (MkRule { iMCT = a })) = (iMCT i) >= a

-- | An intermediate decision tree of minimum connecting times.
type Tree = DT.Tree MinMCT

-- | A container for Int attribues.
type IntStore = IM.IntMap Tree

emptyInt :: IntStore
emptyInt = IM.empty

storeInt :: IntStore -> Int -> (Tree -> Tree) -> IntStore
storeInt s k c = IM.insertWith f k (c DT.Empty) s
  where f _ = c

fetchInt :: IntStore -> Int -> Tree
fetchInt s k = IM.findWithDefault DT.Empty k s

existAttr1 :: MinMCT -> Bool
existAttr1 = isJust . iAttr1 . getMinMCT

keyAttr1 :: MinMCT -> Int
keyAttr1 = fromJust . iAttr1 . getMinMCT

data Attr1

instance DT.Attribute MinMCT Attr1 where
  data Store_ MinMCT Attr1 = MkStoreAttr1 IntStore
  empty_ = MkStoreAttr1 emptyInt
  store_ (MkStoreAttr1 s) k c = MkStoreAttr1 $ storeInt s (keyAttr1 k) c
  fetch_ (MkStoreAttr1 s) k = fetchInt s (keyAttr1 k)
  exist_ _ = existAttr1

s1 = DT.MkStorable (DT.empty :: DT.Store MinMCT Attr1)

-- | Create a decision tree from a list of rules.
fromList :: [Rule] -> MCTTree
fromList xs = DT.fromList [s1] $ map MinMCT xs
