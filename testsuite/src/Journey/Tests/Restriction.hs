module Journey.Tests.Restriction (tests) where

import Data.Maybe (isJust)

import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

import Journey.Restriction

l1  = Local 1 False NoRestriction False False
l2  = Local 1 False NoLocal       False False

l3  = Local 1 False QOnlineCnx    False True
l4  = Local 1 False QOnlineCnx    False False
l5  = Local 1 False QOnlineCnx    True  False
l11 = Local 1 False QOnlineCnx    True  True

l6  = Local 1 False OnlineCnx     False True
l7  = Local 1 False OnlineCnx     False False
l8  = Local 1 False OnlineCnx     True  False

l9  = Local 1 True  NoRestriction     False False
l10 = Local 1 False IntlOnlineCnxStop True  False

assertAllowed c = isJust c @?= True
assertDenied c = isJust c @?= False

tests = [
    testCase "NoRestriction" . assertAllowed $
      initiate l1 >>= connect l1 >>= complete
  , testCase "NoLocal" . assertDenied $
      initiate l1 >>= connect l2 >>= complete
  , testCase "QOnlineCnx_9" . assertDenied $
      initiate l3 >>= connect l5 >>= complete
  , testCase "QOnlineCnx_10" . assertDenied $
      initiate l3 >>= connect l4 >>= complete
  , testCase "QOnlineCnx_11" . assertDenied $
      initiate l4 >>= connect l4 >>= complete
  , testCase "QOnlineCnx_12" . assertAllowed $
      initiate l3 >>= connect l3 >>= connect l5 >>= complete
  , testCase "QOnlineCnx_13" . assertDenied $
      initiate l3 >>= connect l4 >>= connect l5 >>= complete
  , testCase "OnlineCnx_14" . assertAllowed $
      initiate l6 >>= connect l7 >>= connect l8 >>= complete
  , testCase "IntlOnlineCnxStop_15" . assertAllowed $
      initiate l1 >>= connect l9 >>= connect l10 >>= complete
  , testCase "QOnlineCnx_16" . assertDenied $
      initiate l11 >>= connect l11 >>= connect l9 >>= complete
  , testCase "QOnlineCnx_17" . assertAllowed $
      initiate l6 >>= connect l11 >>= connect l5 >>= complete
  ]
