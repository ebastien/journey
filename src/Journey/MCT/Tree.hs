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
  mempty = MkMinMCT $ MkRule undefMCT undefined
                      undefined undefined undefined
  mappend i@(MkMinMCT (MkRule { rMCT = a }))
          j@(MkMinMCT (MkRule { rMCT = b })) | a < b     = i
                                             | otherwise = j

-- | The Rule monoid under maximum rank.
newtype MaxRank = MkMaxRank { getMaxRank :: Rule }
                            deriving (Show)

instance Monoid MaxRank where
  mempty = MkMaxRank $ MkRule undefined undefRank
                       undefined undefined undefined
  mappend i@(MkMaxRank (MkRule { rRank = x }))
          j@(MkMaxRank (MkRule { rRank = y })) | x > y     = i
                                               | otherwise = j

-- | A decision tree on minimum connecting times.
type MCTTree = DT.Tree MinMCT

-- | The 'pruneLookup' function returns the best matching element
--   or nothing, discarding branches by MCT.
pruneLookup :: MCTTree -> Rule -> Maybe Rule
pruneLookup t i = if notfound; then Nothing; else Just result
  where result = getMaxRank $ DT.lookupWith mlift keep t (MkMinMCT i)
        notfound = rRank result == undefRank
        mlift (MkMinMCT j) = MkMaxRank j
        keep (MkMinMCT (MkRule { rMCT = a })) = (rMCT i) >= a

-- | An existential container for minimum connecting times.
type MCTStorable = DT.Storable MinMCT

-- | Create a decision tree from a list of rules.
fromList :: [MCTStorable] -> [Rule] -> MCTTree
fromList s rs = DT.fromList s $ map MkMinMCT rs
