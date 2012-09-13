module Journey.Tests.MinIntTree where

import qualified Journey.MinIntTree as T

attr :: String -> Int -> Maybe Char
attr s n = case s !! n of
             ' ' -> Nothing
             c   -> Just c

prop_retrieve x = T.lookup t r == [n]
  where t = T.fromList [(r, n)]
        r = map (attr x) . take n $ [0..]
        n = length x

