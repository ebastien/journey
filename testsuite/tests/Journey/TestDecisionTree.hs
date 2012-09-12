module Journey.TestDecisionTree where

import qualified Journey.DecisionTree as DT

attr :: Int -> String -> Maybe Char
attr n s = case s !! n of
             ' ' -> Nothing
             c   -> Just c

prop_retrieve x = DT.lookup t x == [x]
  where t = DT.fromList r [x]
        r = map attr . take (length x) $ [0..]

