module Journey.MCTTree (
    lookup
  , empty
  , fromList
  , MCTTree
  , MCT(..)
  , Rank(..)
  ) where

import Data.Monoid (Monoid(..))
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
type MCTTree b = DT.Tree MinMCT b

lookup :: (Ord b) => MCTTree b -> DT.Rule b -> MCT -> Maybe Item
lookup t r k = let i@(_, x) = getMaxRank $ DT.lookupWith l p t r
               in if x == minBound; then Nothing; else Just i
  where l (MinMCT i)      = MaxRank i
        p (MinMCT (a, _)) = k > a

-- | The empty decision tree.
empty :: MCTTree b
empty = DT.empty

-- | Create a decision tree from a list of rules and items associations.
fromList :: (Ord b) => [(DT.Rule b, Item)] -> MCTTree b
fromList xs = DT.fromList $ map m xs
  where m (r, i) = (r, MinMCT i)

