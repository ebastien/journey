module Journey.Tests.Restriction (tests) where

import Data.Maybe (isJust)

import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

import Journey.Restriction

l1  = Local (False, False) (Transfer 1 False NoRestriction)
l2  = Local (False, False) (Transfer 1 False NoLocal)
l3  = Local (False, True ) (Transfer 1 False QOnlineCnx)
l4  = Local (False, False) (Transfer 1 False QOnlineCnx)
l5  = Local (True , False) (Transfer 1 False QOnlineCnx)
l6  = Local (False, True ) (Transfer 1 False OnlineCnx)
l7  = Local (False, False) (Transfer 1 False OnlineCnx)
l8  = Local (True , False) (Transfer 1 False OnlineCnx)
l9  = Local (False, False) (Transfer 1 True  NoRestriction)
l10 = Local (True , False) (Transfer 1 False IntlOnlineCnxStop)
l11 = Local (True , True ) (Transfer 1 False QOnlineCnx)
l12 = Local (False, False) (Transfer 2 False NoRestriction)
l13 = Local (False, False) (Transfer 2 False Connection)

assertAllowed c = isJust c @?= True
assertDenied c = isJust c @?= False

tests = [
    testCase "NoRestriction" . assertAllowed $
      initiate l1 >>= connect l1 >>= finalize
  , testCase "NoLocal" . assertDenied $
      initiate l1 >>= connect l2 >>= finalize
  , testCase "QOnlineCnx_1" . assertDenied $
      initiate l13 >>= finalize

  , testCase "QOnlineCnx_9" . assertDenied $
      initiate l3 >>= connect l5 >>= finalize
  , testCase "QOnlineCnx_10" . assertDenied $
      initiate l3 >>= connect l4 >>= finalize
  , testCase "QOnlineCnx_11" . assertDenied $
      initiate l4 >>= connect l4 >>= finalize
  , testCase "QOnlineCnx_12" . assertAllowed $
      initiate l3 >>= connect l3 >>= connect l5 >>= finalize
  , testCase "QOnlineCnx_13" . assertDenied $
      initiate l3 >>= connect l4 >>= connect l5 >>= finalize
  , testCase "OnlineCnx_14" . assertAllowed $
      initiate l6 >>= connect l7 >>= connect l8 >>= finalize
  , testCase "IntlOnlineCnxStop_15" . assertAllowed $
      initiate l1 >>= connect l9 >>= connect l10 >>= finalize
  , testCase "QOnlineCnx_16" . assertDenied $
      initiate l11 >>= connect l11 >>= connect l9 >>= finalize
  , testCase "QOnlineCnx_17" . assertAllowed $
      initiate l6 >>= connect l11 >>= connect l5 >>= finalize
  , testCase "QOnlineCnx_A" . assertAllowed $
      initiate l1 >>= connect l4 >>= finalize
  , testCase "QOnlineCnx_B" . assertAllowed $
      initiate l1 >>= connect l4 >>= connect l4 >>= finalize
  , testCase "QOnlineCnx_C" . assertDenied $
      initiate l4 >>= connect l4 >>= finalize
  , testCase "QOnlineCnx_D" . assertDenied $
      initiate l4 >>= connect l4 >>= connect l4 >>= finalize
  , testCase "QOnlineCnx_E" . assertAllowed $
      initiate l12 >>= connect l1 >>= finalize
  , testCase "QOnlineCnx_F" . assertAllowed $
      initiate l12 >>= connect l1 >>= connect l4 >>= finalize
  , testCase "QOnlineCnx_G" . assertDenied $
      initiate l12 >>= connect l1 >>= connect l4 >>= connect l12 >>= finalize
  ]
