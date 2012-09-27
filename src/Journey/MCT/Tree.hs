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
data MinMCT = MkMinMCT { mmMCT :: MCT
                       , mmRank :: Rank } deriving (Show)

instance Monoid MinMCT where
  mempty = MkMinMCT undefMCT undefRank
  mappend i@(MkMinMCT a _) j@(MkMinMCT b _) | a < b     = i
                                            | otherwise = j

instance DT.Monoidable Rule where
  type DepthMonoid Rule = MinMCT
  mdepth r = MkMinMCT (rMCT r) (rRank r)

-- | The Rule monoid under maximum rank.
data MaxRank = MkMaxRank { mrRank :: Rank
                         , mrMCT :: MCT } deriving (Show)

instance Monoid MaxRank where
  mempty = MkMaxRank undefRank undefMCT
  mappend i@(MkMaxRank x _) j@(MkMaxRank y _) | x > y     = i
                                              | otherwise = j

-- | A decision tree on minimum connecting times.
type MCTTree = DT.Tree Rule

-- | The 'pruneLookup' function returns the best matching element
--   or nothing, discarding branches by MCT.
pruneLookup :: MCTTree -> Rule -> Maybe MCT
pruneLookup tree rule = if notfound; then Nothing; else Just $ mrMCT result
  where result = DT.lookupWith mleaf keep tree rule
        notfound = mrRank result == undefRank
        mleaf (MkMinMCT a b) = MkMaxRank b a
        keep (MkMinMCT a _) = rMCT rule >= a

-- | An existential container for minimum connecting times.
type MCTStorable = DT.Storable Rule

-- | Create a decision tree from a list of rules.
fromList :: [MCTStorable] -> [Rule] -> MCTTree
fromList s rs = DT.fromList s rs
