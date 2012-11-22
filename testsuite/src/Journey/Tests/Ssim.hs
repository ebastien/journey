{-# LANGUAGE OverloadedStrings #-}

module Journey.Tests.Ssim (tests) where

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B

import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

import Journey.Types
import Journey.Restriction
import Journey.SegmentPeriod
import Journey.LegPeriod
import Journey.Flight
import Journey.Parsers
import Journey.Ssim.Internal

tests = [
    test_parser
  ]

noAssertion :: a -> Assertion
noAssertion = const $ return ()

l1 = ( "Leg operating"
     , "3 AA    12501J07OCT1207OCT12      7 JFK09000900-04008 LAX12051205-07004 762FAJRDIYBHKMVWNSQLGO      XX                 DDS                                       O                              00000195\n"
     , do l <- legPeriodP
          let f = lpFlight l
          return $ do
            fAirline f @?= read "AA "
            fNumber f  @?= 1
            lpOperating l @?= True
     )

l2 = ( "Leg with traffic restriction"
     , "3 AA 76750101J28JUL1228JUL12     6  SCL10151015-0400  CPO11501150-0400  320YBHKMVWNSQLGO            XX                 DDS      LU                  ZAYY         M                              00987059\n"
     , do l <- legPeriodP
          return $ lpRestrictionAt (mkLegSequence 1) l @?= Just NoLocal
     )

l3 = ( "Leg with codeshare"
     , "3 9W 71090901J23OCT1326OCT13  3456  BOM06300630+05301AIDR07500750+0530  73GYMTUNLQSKHVOW            XX                 DD       S2                  Z            M                              00041791\n"
     , do l <- legPeriodP
          return $ lpOperating l @?= False
     )

s1 = ( "Segment"
     , "4 AA    13701J              AB010JFKLAXAS 1979 /HU 8901 /LY 8144 /QF 3100                                                                                                                         000292\n"
     , do s <- segmentP
          return $ (Just $ dBoard s) @?= toPort "JFK"
     )

s2 = ( "Segment with traffic restriction data element"
     , "4 AS 80100101J              AB170LAXAKLA                                                                                                                                                          522941\n"
     , do s <- segmentP
          return $ dElement s @?= MkDEI17x (mkRestrictPax NoLocal)
     )

fixtures = [ l1, l2, l3, s1, s2 ]

testParse :: P.Parser Assertion -> B.ByteString -> Assertion
testParse p b = case P.parseOnly p b of
                  Left msg -> assertFailure msg
                  Right a  -> a

test_parser = testGroup "Parsing" $ map c fixtures
  where c (m, b, p) = testCase m $ testParse p b
