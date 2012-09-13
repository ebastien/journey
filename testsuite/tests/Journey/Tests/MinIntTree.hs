module Journey.Tests.MinIntTree where

import qualified Journey.MinIntTree as T

attr :: String -> Int -> Maybe Char
attr s n = case s !! n of
             ' ' -> Nothing
             c   -> Just c

prop_retrieve x = T.lookup t r == i
  where t = T.fromList [(r, i)]
        r = map (attr x) . take n $ [0..]
        n = length x
        i = (n, n)

