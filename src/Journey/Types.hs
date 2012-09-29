{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Journey.Types (
      AirlineCode(..)
    , Port(..)
    , OnD
    , POnD(..)
    , Path
    , Dow
    , mkDowValid
    , everyDow
    , Period
    , maxPeriod
    , shiftPeriod
    , intersectPeriods
    , withinPeriod
    , TimeDuration
    , TimeVariation
    , ScheduleTime
    , Flight(..)
    , LegPeriod(..)
    , SegmentLeg(..)
    , SegmentPeriod
    , spPeriod
    , alterPeriod
    , spBoard, spOff
    , spDepartureTime , spArrivalTime , spArrivalDateVariation, spElapsedTime
    , SegmentDate(..)
    , sdBoard, sdOff
    , sdDepartureTime, sdArrivalDate, sdArrivalTime
    , SegmentDEI
    , segmentIdx
    , City, Region(..), State(..), Country(..), TransitArea(..), TransitFlow(..)
    , Terminal(..), AircraftBody(..), AircraftType(..)
    ) where

import Data.Word (Word8)
import Data.Bits (testBit, shift, (.&.), (.|.))
import Data.Char (chr, ord)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

showAlphaPacked :: Int -> Int -> String
showAlphaPacked = loop
  where loop n i | n == 0    = []
                 | otherwise = chr (r + ord 'A') : loop (n-1) q
          where (q,r) = divMod i 26

showAlphaNumPacked :: Int -> Int -> String
showAlphaNumPacked = loop
    where loop n i | n == 0    = []
                   | otherwise = c : loop (n-1) q
            where (q,r) = divMod i 37
                  c | r == 0    = ' '
                    | r < 11    = chr (r -  1 + ord '0')
                    | otherwise = chr (r - 11 + ord 'A')


{-------------------------------------------------------------------------------
  Airline code
-------------------------------------------------------------------------------}

newtype AirlineCode = MkAirlineCode Int deriving (Eq, Ord, Enum)

instance Show AirlineCode where
  show (MkAirlineCode a) = showAlphaNumPacked 3 a

{-------------------------------------------------------------------------------
  Port
-------------------------------------------------------------------------------}

newtype Port = MkPort Int deriving (Eq, Ord, Enum)

instance Show Port where
  show (MkPort p) = showAlphaPacked 3 p

-- | Origin & Destination.
type OnD = (Port, Port)

-- | A sequence of ports.
type Path = [Port]

-- | A packed OnD.
data POnD = MkPOnD !Port !Port deriving (Eq, Show)

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

mkDowValid :: Word8 -> Dow
mkDowValid d = MkDow $ d .&. 0x7F

-- | Lookup a single day of week.
lookupDow :: Int -> Dow -> Bool
lookupDow n (MkDow w) = testBit w (n-1)

everyDow :: Dow
everyDow = MkDow 0x7F

{-------------------------------------------------------------------------------
  Period
-------------------------------------------------------------------------------}

instance Bounded Day where
  minBound = fromGregorian 1900 1 1
  maxBound = fromGregorian 9999 1 1

shiftDate :: Int -> Day -> Day
shiftDate n d = let d' = addDays (fromIntegral n) d
                in min maxBound $ max minBound d'

type Period = (Day, Day, Dow)

-- | Test if a day is within a period.
withinPeriod :: Period -> Day -> Bool
withinPeriod (l,h,o) d = low && high && dow
  where low = d >= l
        high = d <= h
        dow = let (_, _, n) = toWeekDate d in lookupDow n o

maxPeriod :: Period
maxPeriod = (minBound, maxBound, everyDow)

rotateDow_ :: Int -> Word8 -> Word8
rotateDow_ n d = shift d nL .|. shift d nR
  where nL = n `mod` 7
        nR = negate $ (7 - n) `mod` 7

shiftPeriod :: Int -> Period -> Period
shiftPeriod n (d, e, MkDow w) = (d', e', MkDow w')
  where d' = shiftDate n d
        e' = shiftDate n e
        w' = rotateDow_ n w

intersectPeriods :: Period -> Period -> Maybe Period
intersectPeriods (d1, e1, MkDow w1) (d2, e2, MkDow w2) =
  if d <= e && w /= 0; then Just (d, e, MkDow w); else Nothing
  where d = max d1 d2
        e = min e1 e2
        w = w1 .&. w2

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

spBoard :: SegmentPeriod -> Port
spBoard = lpBoard . slLeg . head

spOff :: SegmentPeriod -> Port
spOff = lpOff . slLeg . last

spPeriod :: SegmentPeriod -> Period
spPeriod = lpPeriod . slLeg . head

alterPeriod :: Period -> SegmentPeriod -> SegmentPeriod
alterPeriod p = map u1
  where u1 sl = sl { slLeg = u2 (slLeg sl) }
        u2 lp = lp { lpPeriod = p }

spDepartureTime :: SegmentPeriod -> ScheduleTime
spDepartureTime = lpDepartureTime . slLeg . head

spArrivalTime :: SegmentPeriod -> ScheduleTime
spArrivalTime = lpArrivalTime . slLeg . last

spArrivalDateVariation :: SegmentPeriod -> Int
spArrivalDateVariation s = lpArrivalDateVariation ( slLeg $ last s )
                         - lpDepartureDateVariation ( slLeg $ head s )

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

sdBoard :: SegmentDate -> Port
sdBoard = spBoard . sdSegment

sdOff :: SegmentDate -> Port
sdOff = spOff . sdSegment

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

newtype Country = MkCountry Int deriving (Eq, Ord, Enum)

instance Show Country where
  show (MkCountry p) = showAlphaPacked 2 p

newtype Region = MkRegion Int deriving (Eq, Ord, Enum)

instance Show Region where
  show (MkRegion p) = showAlphaPacked 3 p

newtype State = MkState Int deriving (Eq, Ord, Enum)

instance Show State where
  show (MkState p) = showAlphaPacked 2 p

data TransitArea = Domestic | International deriving (Eq, Ord, Enum, Show)

data TransitFlow = MkTransitFlow TransitArea TransitArea deriving (Eq, Ord, Show)

instance Enum TransitFlow where
  fromEnum (MkTransitFlow a b) = 2 * fromEnum a + fromEnum b
  toEnum i = let (a,b) = divMod i 2 in MkTransitFlow (toEnum a) (toEnum b)

newtype Terminal = MkTerminal Int deriving (Eq, Ord, Enum)

instance Show Terminal where
  show (MkTerminal p) = showAlphaNumPacked 2 p

data AircraftBody = Narrow | Wide deriving (Eq, Ord, Enum, Show)

newtype AircraftType = MkAircraftType Int deriving (Eq, Ord, Enum)

instance Show AircraftType where
  show (MkAircraftType p) = showAlphaNumPacked 3 p
