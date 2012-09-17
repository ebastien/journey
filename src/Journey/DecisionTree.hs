{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Journey.DecisionTree (
      insert
    , lookupWith
    , lookup
    , fromList
    , Tree
    , RootTree
    , StoredTree
    , Rule
    , StoreAttribute(..)
    , Storable(..)
    ) where

import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty)
import Prelude hiding (lookup)

instance Show (Storable k) where
  show _ = "<Storable>"

class StoreAttribute k a where
  data Store k a :: *
  empty :: Store k a
  store :: Tree k -> k -> Store k a -> (Tree k -> Tree k) -> Store k a
  unstore :: Tree k -> k -> Store k a -> Tree k

data Storable k = forall a . StoreAttribute k a => MkStorable (Store k a)

type StoredTree a = Storable a

data RootTree a = RootTree [StoredTree a] (Tree a)

-- | A decision tree.
data Tree a = Node (Tree a) (StoredTree a) a
            | Leaf a
            | Empty
              deriving (Show)

-- | A classification rule.
type Rule = [Bool]

-- | Insert an item into a tree by following a classification rule.
insert :: (Monoid a) => RootTree a
                     -> (Rule, a)
                     -> RootTree a
insert (RootTree defs tree) (rule, item) = RootTree defs $ walk (zip rule defs) tree
  where walk [] _ = Leaf item
        walk ((active, MkStorable s):rs) t
          | active = case t of
              Empty                   -> let m' = store Empty item s (walk rs)
                                         in Node Empty (MkStorable m') item
              Node d (MkStorable m) l -> let m' = store Empty item m (walk rs)
                                             l' = item `mappend` l
                                         in Node d (MkStorable m') l'
          | otherwise = case t of
              Empty      -> Node (walk rs Empty) (MkStorable s) item
              Node d m l -> let l' = item `mappend` l
                            in Node (walk rs d) m l'

-- | Lookup items from a tree by following a classification rule.
lookupWith :: (Monoid m) => (a -> m)
                         -> (a -> Bool)
                         -> RootTree a
                         -> (Rule, a)
                         -> m
lookupWith mmap pred (RootTree _ tree) (rule, item) = walk rule tree
  where walk _           (Leaf i)                           = mmap i
        walk []          (Node d _ l)              | pred l = walk [] d
        walk (active:rs) (Node d (MkStorable m) l) | pred l =
          if active
            then mappend left $ right
            else left
          where left  = walk rs d
                right = walk rs $ unstore Empty item m
        walk _ _ = mempty

-- | Lookup specialized to return the list of all matching items.
lookup :: RootTree a -> (Rule, a) -> [a]
lookup = lookupWith (:[]) $ const True

-- | Create a decision tree from a list of rules and items associations.
fromList :: (Monoid a) => [StoredTree a] -> [(Rule, a)] -> RootTree a
fromList defaults = foldl insert $ RootTree defaults Empty
