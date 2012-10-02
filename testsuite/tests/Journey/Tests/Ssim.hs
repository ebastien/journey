{-# LANGUAGE OverloadedStrings #-}

module Journey.Tests.Ssim (ssimTests) where

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B

import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

import Journey.Ssim.Internal

ssimTests = [
    testCase "legparser" test_legparser
  , testCase "segparser" test_segparser
  ]

l1 = "3 AA    12501J07OCT1207OCT12      7 JFK09000900-04008 LAX12051205-07004 762FAJRDIYBHKMVWNSQLGO      XX                 DDS                                       O                              00000195\n"
s1 = "4 AA    13701J              AB010JFKLAXAS 1979 /HU 8901 /LY 8144 /QF 3100                                                                                                                         000292\n"

testParse :: P.Parser a -> B.ByteString -> (a -> Assertion) -> Assertion
testParse p b a = case P.parseOnly p b of
                    Left msg -> assertFailure msg
                    Right r  -> a r

test_legparser = testParse legPeriodP l1 (const $ return ())

test_segparser = testParse segmentP s1 (const $ return ())
