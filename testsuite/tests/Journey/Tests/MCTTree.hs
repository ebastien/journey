module Journey.Tests.MCTTree where

import Journey.MCTTree

r1 = MkItem (MkMCT 30) (MkRank 1) (Just 10)
r2 = MkItem (MkMCT 40) (MkRank 2) (Just 20)
r3 = MkItem (MkMCT 50) (MkRank 3) Nothing

prop_lookup = pruneLookup t i == Just r3
  where t = fromList [r1, r2, r3]
        i = MkItem (MkMCT 45) undefRank (Just 20)
