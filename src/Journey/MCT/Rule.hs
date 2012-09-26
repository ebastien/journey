module Journey.MCT.Rule (
    MCT(..)
  , undefMCT
  , Rank(..)
  , undefRank
  , Rule(..)
  , Options(..)
  , undefRule
  , mkRule
  , mkQuery
  ) where
  
import Data.Time.Calendar (Day)

import Journey.Types

newtype MCT = MkMCT { getMCT :: Int }
                    deriving (Eq, Ord, Bounded, Show)

undefMCT :: MCT
undefMCT = MkMCT maxBound

newtype Rank = MkRank { getRank :: Int }
                      deriving (Eq, Ord, Bounded, Show)

undefRank :: Rank
undefRank = MkRank minBound

data Rule = MkRule { rRank :: Rank
                   , rMCT :: MCT
                   , rOptions :: Options }
                   deriving (Eq, Show)

data Options = MkOptions { rIntraPort :: Bool
                         , rAirports :: Maybe POnD
                         , rArrTerminal :: Maybe Terminal
                         , rDepTerminal :: Maybe Terminal
                         , rTransitFlow :: TransitFlow
                         , rArrCarrier :: Maybe AirlineCode
                         , rDepCarrier :: Maybe AirlineCode
                         , rPrevRegion :: Maybe Region
                         , rNextRegion :: Maybe Region
                         , rPrevCountry :: Maybe Country
                         , rNextCountry :: Maybe Country
                         , rPrevState :: Maybe State
                         , rNextState :: Maybe State
                         , rPrevCity :: Maybe City
                         , rNextCity :: Maybe City
                         , rPrevPort :: Maybe Port
                         , rNextPort :: Maybe Port
                         , rArrFlights :: Maybe (Int, Int)
                         , rDepFlights :: Maybe (Int, Int)
                         , rArrAircraftBody :: Maybe AircraftBody
                         , rDepAircraftBody :: Maybe AircraftBody
                         , rArrAircraftType :: Maybe AircraftType
                         , rDepAircraftType :: Maybe AircraftType
                         , rValidityBegin :: Maybe Day
                         , rValidityEnd :: Maybe Day 
                         }
                         deriving (Eq, Show)

undefRule :: Rule
undefRule = MkRule undefRank undefMCT undefined

mkRule :: Rank -> MCT -> Options -> Rule
mkRule = MkRule

mkQuery :: MCT -> Options -> Rule
mkQuery = MkRule undefRank
