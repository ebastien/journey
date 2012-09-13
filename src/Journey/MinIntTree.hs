module Journey.MinIntTree (
    lookup
  , empty
  , fromList
  , MinIntTree
  , Min(..)
  ) where

import Data.Monoid (Monoid(..))
import Prelude hiding (lookup)

import qualified Journey.DecisionTree as DT

-- | 
type Item = (Int, Int)

newtype Min = Min { getMin :: Item }

instance Monoid Min where
  mempty = Min (maxBound, undefined)
  (Min (a, x)) `mappend` (Min (b, y)) | a < b     = Min (a, x)
                                      | otherwise = Min (b, y)

newtype Max = Max { getMax :: Item }

instance Monoid Max where
  mempty = Max (undefined, minBound)
  (Max (a, x)) `mappend` (Max (b, y)) | x > y     = Max (a, x)
                                      | otherwise = Max (b, y)

-- | A decision tree of minimum Int values.
type MinIntTree b = DT.Tree Min b

lookup :: (Ord b) => MinIntTree b -> DT.Rule b -> Item
lookup t = getMax . DT.lookupWith l p t
  where l (Min i) = Max i
        p = const True

-- | The empty decision tree.
empty :: MinIntTree b
empty = DT.empty

-- | Create a decision tree from a list of rules and items associations.
fromList :: (Ord b) => [(DT.Rule b, Item)] -> MinIntTree b
fromList xs = DT.fromList $ map m xs
  where m (r, i) = (r, Min i)

