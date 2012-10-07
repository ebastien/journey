module Journey.Tests.MCTTree where

import Data.Time.Calendar (fromGregorian)

import Journey.MCT.Rule
import Journey.MCT.Tree
import Journey.MCT.Attributes

{-
d1 = Just $ fromGregorian 2012  9 19
d2 = Just $ fromGregorian 2012  9 25
d3 = Just $ fromGregorian 2012 10 12
d4 = Just $ fromGregorian 2012 10 26

r0 = mkRule (MkRank 2) (MkMCT 20) $ MkOptions (Just 10) Nothing Nothing
r1 = mkRule (MkRank 3) (MkMCT 30) $ MkOptions (Just 10) Nothing Nothing
r2 = mkRule (MkRank 4) (MkMCT 40) $ MkOptions (Just 10) d1      d4
r5 = mkRule (MkRank 5) (MkMCT 40) $ MkOptions (Just 10) d2      d3
r3 = mkRule (MkRank 2) (MkMCT 50) $ MkOptions Nothing   Nothing Nothing
r4 = mkRule (MkRank 1) (MkMCT 60) $ MkOptions (Just 40) Nothing Nothing

tree = fromList attributes [r0, r1, r2, r3, r4, r5]

prop_lookup1 = pruneLookup tree q == Just r1
  where q = mkQuery (MkMCT 35) $ MkOptions (Just 10) Nothing Nothing

prop_lookup2 = pruneLookup tree q == Just r3
  where q = mkQuery (MkMCT 55) $ MkOptions (Just 30) Nothing Nothing

prop_lookup3 = pruneLookup tree q == Nothing
  where q = mkQuery (MkMCT 45) $ MkOptions (Just 30) Nothing Nothing

prop_lookup4 = pruneLookup tree q == Just r0
  where q = mkQuery (MkMCT 25) $ MkOptions (Just 10) Nothing Nothing

prop_lookup5 = pruneLookup tree q == Just r5
  where q = mkQuery (MkMCT 65) $ MkOptions (Just 10) d d
        d = Just $ fromGregorian 2012 9 29
-}
