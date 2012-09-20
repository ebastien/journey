{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Journey.MCT.Tree (
    pruneLookup
  , fromList
  , MCTTree
  , MinMCT(..)
  , MCTStorable
  ) where

import Data.Monoid (Monoid(..))
import Prelude hiding (lookup)

import qualified Journey.MCT.DecisionTree as DT
import Journey.MCT.Rule

-- | The Rule monoid under minimum connecting time.
newtype MinMCT = MkMinMCT { getMinMCT :: Rule }
                          deriving (Show)

instance Monoid MinMCT where
  mempty = MkMinMCT undefRule
  mappend i@(MkMinMCT a)
          j@(MkMinMCT b) | rMCT a < rMCT b = i
                         | otherwise       = j

-- | The Rule monoid under maximum rank.
newtype MaxRank = MkMaxRank { getMaxRank :: Rule }
                            deriving (Show)

instance Monoid MaxRank where
  mempty = MkMaxRank undefRule
  mappend i@(MkMaxRank x)
          j@(MkMaxRank y) | rRank x > rRank y = i
                          | otherwise         = j

-- | A decision tree on minimum connecting times.
type MCTTree = DT.Tree MinMCT

-- | The 'pruneLookup' function returns the best matching element
--   or nothing, discarding branches by MCT.
pruneLookup :: MCTTree -> Rule -> Maybe Rule
pruneLookup t i = if notfound; then Nothing; else Just result
  where result = getMaxRank $ DT.lookupWith mlift keep t (MkMinMCT i)
        notfound = rRank result == undefRank
        mlift (MkMinMCT j) = MkMaxRank j
        keep (MkMinMCT a) = rMCT i >= rMCT a

-- | An existential container for minimum connecting times.
type MCTStorable = DT.Storable MinMCT

-- | Create a decision tree from a list of rules.
fromList :: [MCTStorable] -> [Rule] -> MCTTree
fromList s rs = DT.fromList s $ map MkMinMCT rs
