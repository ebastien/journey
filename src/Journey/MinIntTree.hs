module Journey.MinIntTree (
    lookup
  , empty
  , fromList
  , MinIntTree
  ) where

import Data.Monoid (Monoid(..))
import Prelude hiding (lookup)

import qualified Journey.DecisionTree as DT

newtype Min a = Min { getMin :: a }
                deriving (Eq, Ord, Read, Show, Bounded)

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound
  Min a `mappend` Min b = Min (a `min` b)

type MinIntTree b = DT.Tree (Min Int) b

-- | Lookup specialized to return the list of all matching items.
lookup :: (Ord b) => MinIntTree b -> DT.Rule b -> [Int]
lookup = undefined

-- | The empty decision tree.
empty :: MinIntTree b
empty = undefined

-- | Create a decision tree from a list of rules and items associations.
fromList :: (Ord b, Monoid a) => [(DT.Rule b, a)] -> MinIntTree b
fromList = undefined
