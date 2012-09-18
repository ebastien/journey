module Journey.Tests.MCTTree where

import Journey.MCTTree

r1 = MkRule (MkMCT 30) (MkRank 3) (Just 10)
r2 = MkRule (MkMCT 40) (MkRank 4) (Just 10)
r3 = MkRule (MkMCT 50) (MkRank 2) Nothing
r4 = MkRule (MkMCT 60) (MkRank 1) (Just 40)

tree = fromList [r1, r2, r3, r4]

prop_lookup1 = pruneLookup tree i == Just r1
  where i = MkRule (MkMCT 35) undefRank (Just 10)

prop_lookup2 = pruneLookup tree i == Just r3
  where i = MkRule (MkMCT 55) undefRank (Just 30)

prop_lookup3 = pruneLookup tree i == Nothing
  where i = MkRule (MkMCT 45) undefRank (Just 30)

prop_lookup4 = pruneLookup tree i == Just r2
  where i = MkRule (MkMCT 65) undefRank (Just 10)

