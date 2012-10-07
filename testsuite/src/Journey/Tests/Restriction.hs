module Journey.Tests.Restriction (tests) where

import Data.Maybe (fromJust)

import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

import Journey.Restriction

tests = [
    test1
  , test2
  ]

t1 = Traffic (Cnx 1 False NoRestriction)
             (Cnx 1 False NoRestriction)
             (False, False)

t2 = Traffic (Cnx 1 False NoRestriction)
             (Cnx 1 False NoLocal)
             (False, False)

c11 = connect t1 =<< Just t1
c12 = connect t1 =<< Just t2

test1 = testCase "Simple" $ c11 @?= Just t1
test2 = testCase "NoLocal" $ (complete =<< c12) @?= Nothing

