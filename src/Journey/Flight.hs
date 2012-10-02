module Journey.Flight (
    Flight(..)
  ) where

import Journey.Types

-- | A flight period designator.
data Flight = Flight { fAirline :: !AirlineCode
                     , fNumber :: !Int
                     , fSuffix :: !Char
                     , fVariation :: !Int
                     } deriving (Show, Eq)
