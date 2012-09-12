module Journey.DecisionTree (
      empty
    , insert
    , lookupWith
    , lookup
    , fromList
    ) where

import qualified Data.Map as M
import Data.Monoid (Monoid, mappend, mempty)
import Prelude hiding (lookup)

import Data.Char (ord, chr)

data Tree a b = Node (Tree a b) (M.Map b (Tree a b))
              | Leaf a
              | Empty
                deriving (Show)

type Attribute a b = (a -> Maybe b)

type Rule a b = [Attribute a b]

data DecisionTree a b = MkDecisionTree (Rule a b) (Tree a b)

instance (Show a, Show b) => Show (DecisionTree a b) where
  show (MkDecisionTree r t) = show t

insert :: (Ord b) => DecisionTree a b -> a -> DecisionTree a b
insert (MkDecisionTree rules tree) item = MkDecisionTree rules (walk rules tree)
  where walk []     _ = Leaf item
        walk (r:rs) t = case r item of
          Nothing -> case t of
                       Empty    -> Node (walk rs Empty) M.empty
                       Node d m -> Node (walk rs d) m
          Just c  -> case t of
                       Empty    -> Node Empty $ M.singleton c (walk rs Empty)
                       Node d m -> let t' = M.findWithDefault Empty c m
                                       t'' = walk rs t'
                                       m' = M.insert c t'' m
                                   in Node d m'

lookupWith :: (Ord b, Monoid m) => (a -> m) -> DecisionTree a b -> a -> m
lookupWith f (MkDecisionTree rules tree) item = walk rules tree
  where walk _      Empty      = mempty
        walk []     (Leaf i)   = f i
        walk (r:rs) (Node d m) = left `mappend` right
          where left  = walk rs d
                right = case r item of
                          Nothing -> mempty
                          Just c  -> walk rs $ M.findWithDefault Empty c m

lookup :: (Ord b) => DecisionTree a b -> a -> [a]
lookup = lookupWith (:[])

empty :: Rule a b -> DecisionTree a b
empty r = MkDecisionTree r Empty

fromList :: (Ord b) => Rule a b -> [a] -> DecisionTree a b
fromList r = foldl insert $ empty r
