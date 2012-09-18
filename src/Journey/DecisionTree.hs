{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Journey.DecisionTree (
      insert
    , lookupWith
    , lookup
    , fromList
    , Tree(..)
    , RootTree
    , Attribute(..)
    , Storable(..)
    , Store(..)
    ) where

import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty)
import Data.Foldable (foldMap)
import Prelude hiding (lookup)

-- | The class of attributes 'a' on elements 'k'.
class Attribute k a where
  -- | The internal container.
  data Store_ k a :: *
  
  empty_ :: Store_ k a
  store_ :: Store_ k a -> k -> (Tree k -> Tree k) -> Store_ k a
  fetch_ :: Store_ k a -> k -> Tree k
  exist_ :: Store_ k a -> k -> Bool
  
  -- | An empty attributes container.
  empty :: Store k a
  empty = MkStore Empty empty_

  -- | The 'store' function inserts an optional attribute to the container.
  store :: Store k a -> k -> (Tree k -> Tree k) -> Store k a
  store (MkStore w s) k c = MkStore w' s'
    where (w', s') = if exist_ s k
                       then (w  , store_ s k c)
                       else (c w, s)
  
  -- | The 'fetch' function retrieves an optional attribute from the container.
  fetch :: Monoid m => Store k a -> k -> (Tree k -> m) -> m
  fetch (MkStore w s) k c = (c w) `mappend` m
    where m = if exist_ s k
                then c $ fetch_ s k
                else mempty

-- | A container for optional attributes.
data Store k a = MkStore (Tree k) (Store_ k a)

-- | An existential container for attributes.
data Storable k = forall a . Attribute k a => MkStorable (Store k a)

-- | A complete decision tree.
data RootTree k = RootTree [Storable k] (Tree k)

-- | An intermediate decision tree.
data Tree k = Node (Storable k) k
            | Leaf [k]
            | Empty

-- | Insert an element into a decision tree.
insert :: (Monoid k) => RootTree k -> k -> RootTree k
insert (RootTree defs tree) rule = RootTree defs $ walk defs tree
  where walk [] t =
          let rs = case t of
                     Leaf xs -> xs
                     Empty   -> []
          in Leaf $ rule : rs
        walk (MkStorable d:ds) t =
          let (z, r') = case t of
                          Empty                 -> ( MkStorable . store d rule
                                                   , rule )
                          Node (MkStorable s) r -> ( MkStorable . store s rule
                                                   , rule `mappend` r )
          in Node (z $ walk ds) r'

-- | Lookup matching elements in a tree.
lookupWith :: (Monoid m) => (k -> m)
                         -> (k -> Bool)
                         -> RootTree k
                         -> k
                         -> m
lookupWith mlift pred (RootTree _ tree) rule = walk tree
  where walk (Leaf xs)                        = foldMap mlift $ filter pred xs
        walk (Node (MkStorable s) l) | pred l = fetch s rule walk
        walk _                                = mempty

-- | Returns the list of all matching elements.
lookup :: RootTree k -> k -> [k]
lookup = lookupWith (:[]) $ const True

-- | Creates a decision tree from a structure of attributes and a list of elements.
fromList :: (Monoid k) => [Storable k] -> [k] -> RootTree k
fromList defs = foldl insert $ RootTree defs Empty
