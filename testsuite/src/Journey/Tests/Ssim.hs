{-# LANGUAGE OverloadedStrings #-}

module Journey.Tests.Ssim (tests) where

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B

import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.Time.Clock (secondsToDiffTime)
import Control.Applicative (some)
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

fixtures = [
    ( "Leg operating"
    , "3 AA    12501J07OCT1207OCT12      7 JFK09000900-04008 LAX12051205-07004 762FAJRDIYBHKMVWNSQLGO      XX                 DDS                                       O                              00000195\n"
    , do l <- legPeriodP
         let f = lpFlight l
         return $ do
           fAirline f @?= read "AA "
           fNumber f  @?= 1
           lpOperating l @?= True
           lpDepartureTime l @?= secondsToDiffTime 32400
           lpArrivalTime l @?= secondsToDiffTime 43500
           lpDepartureDateVariation l @?= 0
           lpArrivalDateVariation l @?= 0
           lpElapsedTime l @?= secondsToDiffTime 21900
    ),

    ( "Leg with traffic restriction"
    , "3 AA 76750101J28JUL1228JUL12     6  SCL10151015-0400  CPO11501150-0400  320YBHKMVWNSQLGO            XX                 DDS      LU                  ZAYY         M                              00987059\n"
    , do l <- legPeriodP
         return $ lpRestrictionAt (mkLegSequence 1) l @?= Just NoLocal
    ),

    ( "Leg with codeshare"
    , "3 9W 71090901J23OCT1326OCT13  3456  BOM06300630+05301AIDR07500750+0530  73GYMTUNLQSKHVOW            XX                 DD       S2                  Z            M                              00041791\n"
    , do l <- legPeriodP
         return $ lpOperating l @?= False
    ),

    ( "Segment"
    , "4 AA    13701J              AB010JFKLAXAS 1979 /HU 8901 /LY 8144 /QF 3100                                                                                                                         000292\n"
    , do s <- segmentP
         return $ (Just $ dBoard s) @?= toPort "JFK"
    ),

    ( "Segment with traffic restriction data element"
    , "4 AS 80100101J              AB170LAXAKLA                                                                                                                                                          522941\n"
    , do s <- segmentP
         return $ dElement s @?= MkDEI17x (mkRestrictPax NoLocal)
    ),

    ( "Flight with two legs"
    , "3 AA  1540201J02AUG1220AUG121234567 NRT18151815+09002 ORD16001600-05005 777FAJRDIYBHKMVWNSQLGO      XX                 IIS                                       MM                             00032556\n" `B.append`
      "3 AA  1540202J02AUG1220AUG121234567 ORD17451745-05003 BOS21052105-0400B 738FAPYBHKMLWVSNQOG         XX                 DDS                                        O                             00032563\n"
    , do f <- some legGroupP
         let segs = flightSegments f
         return $ do
           length segs @?= 3
    )
  ]

testParse :: P.Parser Assertion -> B.ByteString -> Assertion
testParse p b = case P.parseOnly p b of
                  Left msg -> assertFailure msg
                  Right a  -> a

test_parser = testGroup "Parsing" $ map c fixtures
  where c (m, b, p) = testCase m $ testParse p b
