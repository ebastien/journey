module Journey.Tests.MCTTree where

import qualified Journey.MCTTree as T

attr :: String -> Int -> Maybe Char
attr s n = case s !! n of
             ' ' -> Nothing
             c   -> Just c

prop_retrieve x = T.lookup t r k == Just i
  where t = T.fromList [(r, i)]
        r = map (attr x) . take n $ [0..]
        n = length x
        i = (T.MCT n, T.Rank n)
        k = T.MCT $ 2 * n
