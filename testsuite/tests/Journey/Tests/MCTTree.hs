module Journey.Tests.MCTTree where

import Data.Time.Calendar (fromGregorian)

import Journey.MCT.Rule
import Journey.MCT.Tree
import Journey.MCT.Attributes

d1 = Just $ fromGregorian 2012  9 19
d2 = Just $ fromGregorian 2012  9 25
d3 = Just $ fromGregorian 2012 10 12
d4 = Just $ fromGregorian 2012 10 26

r0 = MkRule (MkMCT 20) (MkRank 2) (Just 10) Nothing Nothing
r1 = MkRule (MkMCT 30) (MkRank 3) (Just 10) Nothing Nothing
r2 = MkRule (MkMCT 40) (MkRank 4) (Just 10) d1      d4
r5 = MkRule (MkMCT 40) (MkRank 5) (Just 10) d2      d3
r3 = MkRule (MkMCT 50) (MkRank 2) Nothing   Nothing Nothing
r4 = MkRule (MkMCT 60) (MkRank 1) (Just 40) Nothing Nothing

tree = fromList attributes [r0, r1, r2, r3, r4, r5]

prop_lookup1 = pruneLookup tree i == Just r1
  where i = MkRule (MkMCT 35) undefRank (Just 10) Nothing Nothing

prop_lookup2 = pruneLookup tree i == Just r3
  where i = MkRule (MkMCT 55) undefRank (Just 30) Nothing Nothing

prop_lookup3 = pruneLookup tree i == Nothing
  where i = MkRule (MkMCT 45) undefRank (Just 30) Nothing Nothing

prop_lookup4 = pruneLookup tree i == Just r0
  where i = MkRule (MkMCT 25) undefRank (Just 10) Nothing Nothing

prop_lookup5 = pruneLookup tree i == Just r5
  where i = MkRule (MkMCT 65) undefRank (Just 10) d d
        d = Just $ fromGregorian 2012 9 29
