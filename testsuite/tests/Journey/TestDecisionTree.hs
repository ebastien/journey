module Journey.TestDecisionTree () where

import Test.QuickCheck
import qualified Journey.DecisionTree as DT

attr :: Int -> String -> Maybe Char
attr n s = case s !! n of
             ' ' -> Nothing
             c   -> Just c

rules = map attr [0,1,2]

tree = DT.fromList rules ["aix", "bjy", "  z", " k ", "c  "]
