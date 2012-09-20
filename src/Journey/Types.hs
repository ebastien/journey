{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Journey.Types (
      AirlineCode(..)
    , Port(..)
    , OnD
    , Path
    , PeriodBoundary
    , Dow(..)
    , Period
    , withinPeriod
    , TimeDuration
    , TimeVariation
    , ScheduleTime
    , Flight(..)
    , LegPeriod(..)
    , SegmentLeg(..)
    , SegmentPeriod
    , spDepartureTime , spArrivalTime , spArrivalDateVariation, spElapsedTime
    , SegmentDate(..)
    , sdDepartureTime, sdArrivalDate, sdArrivalTime
    , SegmentDEI
    , segmentIdx
    ) where

import Data.Word (Word8)
import Data.Bits (testBit)
import Data.Char (chr, ord)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Calendar (Day, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Control.DeepSeq (NFData)

{-------------------------------------------------------------------------------
  Airline code
-------------------------------------------------------------------------------}

newtype AirlineCode = MkAirlineCode Int deriving (Eq, Ord)

instance Show AirlineCode where
  show (MkAirlineCode a) = loop (3 :: Int) a
    where loop n i | n == 0    = []
                   | otherwise = c : loop (n-1) q
            where (q,r) = divMod i 37
                  c | r == 0    = ' '
                    | r < 11    = chr (r -  1 + ord '0')
                    | otherwise = chr (r - 11 + ord 'A')

{-------------------------------------------------------------------------------
  Port
-------------------------------------------------------------------------------}

newtype Port = MkPort Int deriving (Eq, Ord, Enum, NFData)

instance Show Port where
  show (MkPort p) = loop (3 :: Int) p
    where loop n i | n == 0    = []
                   | otherwise = chr (r + ord 'A') : loop (n-1) q
            where (q,r) = divMod i 26

-- | Origin & Destination.
type OnD = (Port, Port)

-- | A sequence of ports.
type Path = [Port]

{-------------------------------------------------------------------------------
  Days of the week
-------------------------------------------------------------------------------}

newtype Dow = MkDow Word8

instance Show Dow where
  show (MkDow w) = map step [0..6]
    where step n | testBit w n = chr (n + ord '1')
                 | otherwise   = ' '

-- | Lookup a single day of week.
lookupDow :: Int -> Dow -> Bool
lookupDow n (MkDow w) = testBit w (n-1)

{-------------------------------------------------------------------------------
  Period
-------------------------------------------------------------------------------}

type PeriodBoundary = Maybe Day

type Period = (Day, PeriodBoundary, Dow)

-- | Test if a day is within a period.
withinPeriod :: Period -> Day -> Bool
withinPeriod (l,h,o) d = low && high && dow
  where low = d >= l
        high = case h of
                 Just h' -> d <= h'
                 Nothing -> True
        dow = let (_, _, n) = toWeekDate d in lookupDow n o

{-------------------------------------------------------------------------------
  Date and time
-------------------------------------------------------------------------------}

type TimeDuration = DiffTime
type TimeVariation = DiffTime
type ScheduleTime = DiffTime

{-------------------------------------------------------------------------------
  Schedule
-------------------------------------------------------------------------------}

-- | A flight period designator.
data Flight = Flight { fAirline :: !AirlineCode
                     , fNumber :: !Int
                     , fSuffix :: !Char
                     , fVariation :: !Int
                     } deriving (Show, Eq)

-- | A leg period.
data LegPeriod = LegPeriod { lpFlight :: Flight
                           , lpPeriod :: !Period
                           , lpSequence :: !Int
                           , lpBoard :: !Port
                           , lpOff :: !Port
                           , lpDepartureTime :: !ScheduleTime
                           , lpArrivalTime :: !ScheduleTime
                           , lpElapsedTime :: !TimeDuration
                           , lpDepartureDateVariation :: !Int
                           , lpArrivalDateVariation :: !Int
                           } deriving Show

-- | Segment data element.
type SegmentDEI = Int

-- | Segment index from leg sequences.
segmentIdx :: Int -> Int -> Int
segmentIdx board off = off * 26 + board - 1

-- | The projection of a leg to a specific segment.
data SegmentLeg = MkSegmentLeg { slLeg :: LegPeriod
                               , slDEIs :: [SegmentDEI]
                               } deriving (Show)

-- | A segment as a sequence of legs periods.
type SegmentPeriod = [SegmentLeg]

spDepartureTime :: SegmentPeriod -> ScheduleTime
spDepartureTime = lpDepartureTime . slLeg . head

spArrivalTime :: SegmentPeriod -> ScheduleTime
spArrivalTime = lpArrivalTime . slLeg . last

spArrivalDateVariation :: SegmentPeriod -> Int
spArrivalDateVariation = lpArrivalDateVariation . slLeg . last

spElapsedTime :: SegmentPeriod -> TimeDuration
spElapsedTime s = (lpElapsedTime $ head legs) + (sum . map cnx . zip legs $ tail legs)
  where legs = map slLeg s
        cnx (a,b) = lpElapsedTime b
                  + lpDepartureTime b - lpArrivalTime a
                  + ( secondsToDiffTime . fromIntegral
                    $ ( lpDepartureDateVariation b
                      - lpArrivalDateVariation a ) * 86400 )

-- | A segment date.
data SegmentDate = MkSegmentDate { sdSegment :: SegmentPeriod
                                 , sdDepartureDate :: Day
                                 } deriving (Show)

sdDepartureTime :: SegmentDate -> ScheduleTime
sdDepartureTime = spDepartureTime . sdSegment

sdArrivalDate :: SegmentDate -> Day
sdArrivalDate s = addDays (fromIntegral . spArrivalDateVariation $ sdSegment s)
                $ sdDepartureDate s

sdArrivalTime :: SegmentDate -> ScheduleTime
sdArrivalTime = spArrivalTime . sdSegment

