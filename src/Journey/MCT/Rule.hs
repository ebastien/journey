module Journey.MCT.Rule (
    MCT(..)
  , undefMCT
  , Rank(..)
  , undefRank
  , Rule(..)
  ) where
  
import Data.Time.Calendar (Day)

newtype MCT = MkMCT { getMCT :: Int }
                    deriving (Eq, Ord, Bounded, Show)

undefMCT :: MCT
undefMCT = MkMCT maxBound

newtype Rank = MkRank { getRank :: Int }
                      deriving (Eq, Ord, Bounded, Show)

undefRank :: Rank
undefRank = MkRank minBound

data Rule = MkRule { rMCT :: MCT
                   , rRank :: Rank
                   , rInt :: Maybe Int
                   , rDay1 :: Maybe Day
                   , rDay2 :: Maybe Day }
                   deriving (Eq, Show)
