{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Journey.Types (
      AirlineCode(..)
    , Port(..)
    , OnD
    , POnD(..)
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
    , City, Region(..), State(..), Country(..), TransitArea(..), TransitFlow(..)
    , Terminal(..), AircraftBody(..), AircraftType(..)
    , TransitPorts(..), otherTransitPort
    ) where

import Data.Word (Word8)
import Data.Bits (testBit, shiftL, shiftR, (.&.))
import Data.Char (chr, ord)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Calendar (Day, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)

{-------------------------------------------------------------------------------
  Airline code
-------------------------------------------------------------------------------}

newtype AirlineCode = MkAirlineCode Int deriving (Eq, Ord, Enum)

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

newtype Port = MkPort Int deriving (Eq, Ord, Enum)

instance Show Port where
  show (MkPort p) = loop (3 :: Int) p
    where loop n i | n == 0    = []
                   | otherwise = chr (r + ord 'A') : loop (n-1) q
            where (q,r) = divMod i 26

-- | Origin & Destination.
type OnD = (Port, Port)

-- | A sequence of ports.
type Path = [Port]

-- | A packed OnD.
data POnD = MkPOnD !Port !Port deriving (Show)

instance Enum POnD where
  fromEnum (MkPOnD a b) = (fromEnum a) * (26^(3::Int)) + (fromEnum b)
  toEnum i = let (a,b) = divMod i (26^(3::Int)) in MkPOnD (toEnum a) (toEnum b)

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

{-------------------------------------------------------------------------------
  Work in progress
-------------------------------------------------------------------------------}

type City = Port

newtype Country = MkCountry Int deriving (Eq, Ord, Enum, Show)

newtype Region = MkRegion Int deriving (Eq, Ord, Enum, Show)

newtype State = MkState Int deriving (Eq, Ord, Enum, Show)

data TransitArea = Domestic | International deriving (Eq, Ord, Enum, Show)

data TransitFlow = MkTransitFlow TransitArea TransitArea deriving (Eq, Ord, Show)

instance Enum TransitFlow where
  fromEnum (MkTransitFlow a b) = 2 * fromEnum a + fromEnum b
  toEnum i = let (a,b) = divMod i 2 in MkTransitFlow (toEnum a) (toEnum b)

newtype Terminal = MkTerminal Int deriving (Eq, Ord, Enum, Show)

data AircraftBody = Narrow | Wide deriving (Eq, Ord, Enum, Show)

newtype AircraftType = MkAircraftType Int deriving (Eq, Ord, Enum, Show)

{-------------------------------------------------------------------------------
  Airports in transit
-------------------------------------------------------------------------------}

data TransitPorts = SameTransitPort (Maybe Port)
                  | OtherTransitPort (Maybe (Port, Port))
                    deriving (Eq, Show)

otherTransitPort :: Port -> Port -> TransitPorts
otherTransitPort a b = OtherTransitPort $ Just (a, b)

instance Enum TransitPorts where
  fromEnum (SameTransitPort Nothing)       = 0
  fromEnum (OtherTransitPort Nothing)      = 1
  fromEnum (SameTransitPort (Just a))      = let n = fromEnum a
                                             in 2 + shiftL n 2
  fromEnum (OtherTransitPort (Just (a,b))) = let n = (fromEnum a) * 26^(3::Int)
                                                   + fromEnum b
                                             in 3 + shiftL n 2

  toEnum i = case (i .&. 3, shiftR i 2) of
               (0, _) -> SameTransitPort Nothing
               (1, _) -> OtherTransitPort Nothing
               (2, n) -> SameTransitPort . Just $ toEnum n
               (3, n) -> let (a,b) = divMod (shiftR i 2) (26^(3::Int))
                         in OtherTransitPort . Just $ (toEnum a, toEnum b)
