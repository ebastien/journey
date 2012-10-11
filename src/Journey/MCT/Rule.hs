{-# LANGUAGE GADTs #-}

module Journey.MCT.Rule (
    MCT(..)
  , undefMCT
  , fromTimes
  , Rank(..)
  , undefRank
  , Rule(..)
  , FlightRange(..)
  , Option(..)
  , fromOption
  , isKnown
  , Options(..)
  , defaultOptions
  , mkRule
  , mkQuery
  ) where

import Data.Time.Calendar (Day)

import Journey.Types

newtype MCT = MkMCT { getMCT :: Int }
                    deriving (Eq, Ord, Bounded, Show)

undefMCT :: MCT
undefMCT = MkMCT maxBound

fromTimes :: ScheduleTime -> ScheduleTime -> MCT
fromTimes a b = MkMCT . round $ (b - a) / 60

newtype Rank = MkRank { getRank :: Int }
                      deriving (Eq, Ord, Bounded, Show)

undefRank :: Rank
undefRank = MkRank minBound

data Rule = MkRule { rRank :: Rank
                   , rMCT :: MCT
                   , rOptions :: Options }
                   deriving (Eq, Show)

data FlightRange = MkFlightRange Int Int deriving (Eq, Show)

data Option a = Unknown | Known a
                deriving (Eq, Show)

fromOption :: Option a -> Maybe a
fromOption o = case o of
                 Unknown -> Nothing
                 Known a -> Just a

isKnown :: Option a -> Bool
isKnown Unknown = False
isKnown _       = True

data Options = MkOptions { rIntraPort :: Bool
                         , rAirports :: Option POnD
                         , rArrTerminal :: Option Terminal
                         , rDepTerminal :: Option Terminal
                         , rTransitFlow :: TransitFlow
                         , rArrCarrier :: Option AirlineCode
                         , rDepCarrier :: Option AirlineCode
                         , rPrevRegion :: Option Region
                         , rNextRegion :: Option Region
                         , rPrevCountry :: Option Country
                         , rNextCountry :: Option Country
                         , rPrevState :: Option State
                         , rNextState :: Option State
                         , rPrevCity :: Option City
                         , rNextCity :: Option City
                         , rPrevPort :: Option Port
                         , rNextPort :: Option Port
                         , rArrFlights :: Option FlightRange
                         , rDepFlights :: Option FlightRange
                         , rArrAircraftBody :: Option AircraftBody
                         , rDepAircraftBody :: Option AircraftBody
                         , rArrAircraftType :: Option AircraftType
                         , rDepAircraftType :: Option AircraftType
                         , rValidityBegin :: Option Day
                         , rValidityEnd :: Option Day
                         }
                         deriving (Eq, Show)

defaultOptions :: Bool -> TransitFlow -> Options
defaultOptions i t = MkOptions i Unknown Unknown Unknown t
  Unknown Unknown Unknown Unknown Unknown Unknown Unknown Unknown Unknown Unknown
  Unknown Unknown Unknown Unknown Unknown Unknown Unknown Unknown Unknown Unknown

mkRule :: Rank -> MCT -> Options -> Rule
mkRule = MkRule

mkQuery :: MCT -> Options -> Rule
mkQuery = MkRule undefRank
