module Journey.MCT.Rule (
    MCT(..)
  , undefMCT
  , Rank(..)
  , undefRank
  , Rule(rRank, rMCT, rOptions)
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

data Options = MkOptions { rArrPort :: Maybe Port
                         , rDepPort :: Maybe Port
                         , rArrFlightBegin :: Maybe Int
                         , rArrFlightEnd :: Maybe Int
                         , rDepFlightBegin :: Maybe Int
                         , rDepFlightEnd :: Maybe Int
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
