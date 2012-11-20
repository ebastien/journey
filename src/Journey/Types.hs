{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Journey.Types (
      AirlineCode(..)
    , Port(..)
    , OnD
    , POnD(..)
    , unpackOnD
    , Path
    , TimeDuration
    , TimeVariation
    , ScheduleTime
    , FrequencyRate
    , City
    , Region(..)
    , State(..)
    , Country(..)
    , TransitArea(..)
    , TransitFlow(..)
    , Terminal(..)
    , AircraftBody(..)
    , AircraftType(..)
    ) where

import Data.Char (chr, ord)
import Data.Time.Clock (DiffTime)

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

-- | Convert a packed OnD to an OnD.
unpackOnD :: POnD -> OnD
unpackOnD (MkPOnD a b) = (a,b)

instance Enum POnD where
  fromEnum (MkPOnD a b) = (fromEnum a) * (26^(3::Int)) + (fromEnum b)
  toEnum i = let (a,b) = divMod i (26^(3::Int)) in MkPOnD (toEnum a) (toEnum b)

{-------------------------------------------------------------------------------
  Date and time
-------------------------------------------------------------------------------}

type TimeDuration = DiffTime
type TimeVariation = DiffTime
type ScheduleTime = DiffTime
type FrequencyRate = Int

{-------------------------------------------------------------------------------
  Locations
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

newtype Terminal = MkTerminal Int deriving (Eq, Ord, Enum)

instance Show Terminal where
  show (MkTerminal p) = showAlphaNumPacked 2 p

{-------------------------------------------------------------------------------
  Transit on connections
-------------------------------------------------------------------------------}

data TransitArea = Domestic | International deriving (Eq, Ord, Enum, Show)

data TransitFlow = MkTransitFlow { transitBoard :: TransitArea
                                 , transitOff :: TransitArea
                                 } deriving (Eq, Ord, Show)

instance Enum TransitFlow where
  fromEnum (MkTransitFlow a b) = 2 * fromEnum a + fromEnum b
  toEnum i = let (a,b) = divMod i 2 in MkTransitFlow (toEnum a) (toEnum b)

{-------------------------------------------------------------------------------
  Aircraft types and categories
-------------------------------------------------------------------------------}

data AircraftBody = Narrow | Wide deriving (Eq, Ord, Enum, Show)

newtype AircraftType = MkAircraftType Int deriving (Eq, Ord, Enum)

instance Show AircraftType where
  show (MkAircraftType p) = showAlphaNumPacked 3 p
