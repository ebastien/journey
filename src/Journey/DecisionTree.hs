module Journey.DecisionTree (
      empty
    , insert
    , lookupWith
    , lookup
    , fromList
    , Tree
    , Rule
    ) where

import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty)
import Prelude hiding (lookup)

-- | A decision tree.
data Tree a b = Node (Tree a b) (M.Map b (Tree a b)) a
              | Leaf a
              | Empty
                deriving (Show)

-- | A classification rule.
type Rule b = [Maybe b]

-- | Insert an item into a tree by following a classification rule.
insert :: (Ord b, Monoid a) => Tree a b
                            -> (Rule b, a)
                            -> Tree a b
insert tree (rule, item) = walk rule tree
  where walk []     _ = Leaf item
        walk (r:rs) t = case r of
          Nothing -> case t of
                       Empty      -> Node (walk rs Empty) M.empty item
                       Node d m l -> let l' = item `mappend` l
                                     in Node (walk rs d) m l'
          Just c  -> case t of
                       Empty      -> Node Empty (M.singleton c (walk rs Empty)) item
                       Node d m l -> let t'  = M.findWithDefault Empty c m
                                         t'' = walk rs t'
                                         m'  = M.insert c t'' m
                                         l'  = item `mappend` l
                                     in Node d m' l'

-- | Lookup items from a tree by following a classification rule.
lookupWith :: (Ord b, Monoid m) => (a -> m)
                                -> (a -> Bool)
                                -> Tree a b
                                -> Rule b
                                -> m
lookupWith mmap pred tree rule = walk rule tree
  where walk _      (Leaf i)              = mmap i
        walk []     (Node d _ l) | pred l = walk [] d
        walk (r:rs) (Node d m l) | pred l = case r of
                                              Nothing -> left
                                              Just c  -> mappend left $ right c
          where left    = walk rs d
                right c = walk rs $ M.findWithDefault Empty c m
        walk _ _ = mempty

-- | Lookup specialized to return the list of all matching items.
lookup :: (Ord b) => Tree a b -> Rule b -> [a]
lookup = lookupWith (:[]) $ const True

-- | The empty decision tree.
empty :: Tree a b
empty = Empty

-- | Create a decision tree from a list of rules and items associations.
fromList :: (Ord b, Monoid a) => [(Rule b, a)] -> Tree a b
fromList = foldl insert Empty
