module Journey.LegPeriod (
    ServiceType(..)
  , LegPeriod(..)
  , lpRestrictionAt
  , LegRestrictions
  , LegSequence
  , mkLegSequence
  , getSequence
  ) where

import Journey.Period
import Journey.Flight
import Journey.Restriction
import Journey.Types

data ServiceType = ServicePax | ServiceCargo | ServiceOther
                   deriving (Show, Eq)

type LegRestrictions = [Restriction]

newtype LegSequence = MkLegSequence { getSequence :: Int }
                      deriving (Show, Eq, Ord)

mkLegSequence :: Int -> LegSequence
mkLegSequence n | n > 0 = MkLegSequence n
mkLegSequence _         = error "Invalid leg sequence number"

-- | A leg period.
data LegPeriod = LegPeriod { lpFlight :: !Flight
                           , lpPeriod :: !Period
                           , lpSequence :: !LegSequence
                           , lpBoard :: !Port
                           , lpOff :: !Port
                           , lpDepartureTime :: !ScheduleTime
                           , lpArrivalTime :: !ScheduleTime
                           , lpElapsedTime :: !TimeDuration
                           , lpDepartureDateVariation :: !Int
                           , lpArrivalDateVariation :: !Int
                           , lpService :: !ServiceType
                           , lpFrequency :: !FrequencyRate
                           , lpDepartureTerminal :: !(Maybe Terminal)
                           , lpArrivalTerminal :: !(Maybe Terminal)
                           , lpAircraftType :: !AircraftType
                           , lpTransitFlow :: !TransitFlow
                           , lpRestrictions :: !LegRestrictions
                           } deriving (Show)

lpRestrictionAt :: LegSequence -> LegPeriod -> Restriction
lpRestrictionAt (MkLegSequence n) l = (lpRestrictions l) !! (n-1)
