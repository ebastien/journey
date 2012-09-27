{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Journey.MCT.DecisionTree (
      lookupWith
    , lookup
    , fromList
    , empty
    , Tree(..)
    , IsAttribute(..), IsStore(..)
    , Storable(..)
    , Store(..)
    ) where

import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty)
import Data.List (foldl')
import Data.Foldable (foldMap)
import Prelude hiding (lookup)

-- | The class of containers of options 'o' on elements 'k'.
class IsStore k o where
  data OptionStore k o :: *
  emptyStore :: OptionStore k o
  storeOption :: OptionStore k o -> o -> (Tree k -> Tree k) -> OptionStore k o
  fetchOption :: OptionStore k o -> o -> [Tree k]

-- | The class of attributes 'a' on elements 'k'.
class IsStore k (Option k a) => IsAttribute k a where
  type Option k a :: *
  maybeOption :: a -> k -> Maybe (Option k a)

-- | A container for optional attributes 'a' on elements 'k'.
data Store k a = MkStore a (Tree k) (OptionStore k (Option k a))

-- instance (Show k, Show (OptionStore k (Option k a))) => Show (Store k a) where
--   show (MkStore t s) = "MkStore " ++ show t ++ " " ++ show s

-- | An empty container.
empty :: IsAttribute k a => a -> Store k a
empty a = MkStore a Empty emptyStore

-- | The 'store' function inserts an optional attribute to the container.
store :: IsAttribute k a => Store k a -> k -> (Tree k -> Tree k) -> Store k a
store (MkStore a w s) k c = MkStore a w' s'
  where (w', s') = case maybeOption a k of
                     Just o  -> (w  , storeOption s o c)
                     Nothing -> (c w, s)

-- | The 'fetch' function retrieves an optional attribute from the container.
fetch :: (IsAttribute k a, Monoid m) => Store k a -> k -> (Tree k -> m) -> m
fetch (MkStore a w s) k c = (c w) `mappend` m
  where m = case maybeOption a k of
              Just o  -> foldMap c $ fetchOption s o
              Nothing -> mempty

-- | An existential container for elements 'k'.
data Storable k = forall a . IsAttribute k a => MkStorable !(Store k a)

-- instance Show k => Show (Storable k) where
--   show (MkStorable s) = show s

-- | A decision tree.
data Tree k = Node !(Storable k) !k
            | Leaf [k]
            | Empty
--            deriving (Show)

-- | Insert an element into a decision tree.
insert :: (Monoid k) => [Storable k] -> Tree k -> k -> Tree k
insert defs tree rule = walk defs tree
  where walk [] t =
          let rs = case t of
                     Leaf xs -> xs
                     Empty   -> []
          in Leaf $! rule : rs
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
                         -> Tree k
                         -> k
                         -> m
lookupWith mlift pred tree rule = walk tree
  where walk (Leaf xs)                        = foldMap mlift $ filter pred xs
        walk (Node (MkStorable s) l) | pred l = fetch s rule walk
        walk _                                = mempty

-- | Returns the list of all matching elements.
lookup :: Tree k -> k -> [k]
lookup = lookupWith (:[]) $ const True

-- | Creates a decision tree from a structure of attributes and a list of elements.
fromList :: (Monoid k) => [Storable k] -> [k] -> Tree k
fromList defs = foldl' (insert defs) Empty
