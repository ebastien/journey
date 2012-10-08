module Journey.Tests.Restriction (tests) where

import Data.Maybe (isJust)

import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

import Journey.Restriction

l1 = Local 1 False NoRestriction False False
l2 = Local 1 False NoLocal False False

l3 = Local 1 False QOnlineCnx False True
l4 = Local 1 False QOnlineCnx False False
l5 = Local 1 False QOnlineCnx True False

assertAllowed c = isJust c @?= True
assertDenied c = isJust c @?= False

tests = [
    testCase "NoRestriction" . assertAllowed $ initiate l1 >>= connect l1 >>= complete
  , testCase "NoLocal"       . assertDenied  $ initiate l1 >>= connect l2 >>= complete
  , testCase "QOnlineCnx_09" . assertDenied  $ initiate l3 >>= connect l5 >>= complete
  , testCase "QOnlineCnx_10" . assertDenied  $ initiate l3 >>= connect l4 >>= complete
  , testCase "QOnlineCnx_11" . assertDenied  $ initiate l4 >>= connect l4 >>= complete
  , testCase "QOnlineCnx_12" . assertAllowed $ initiate l3 >>= connect l3 >>= connect l5 >>= complete
  , testCase "QOnlineCnx_13" . assertDenied  $ initiate l3 >>= connect l4 >>= connect l5 >>= complete
  ]
