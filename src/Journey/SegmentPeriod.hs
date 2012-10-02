module Journey.SegmentPeriod (
    SegmentLeg(..)
  , SegmentPeriod
  , SegmentDEI(..)
  , spFlight
  , spPeriod
  , alterPeriod
  , spBoard, spOff
  , spDepartureTime
  , spArrivalTime
  , spArrivalDateVariation
  , spElapsedTime
  ) where

import Data.Foldable (foldMap)
import Data.Time.Clock (secondsToDiffTime)

import Journey.Types
import Journey.Period
import Journey.Flight
import Journey.LegPeriod
import Journey.Restriction

-- | Segment data element.
data SegmentDEI = MkDEI17x !RestrictService
                | MkDEI71x !RestrictQualifier
                | UnknownDEI
                deriving (Show)

-- | The projection of a leg to a specific segment.
data SegmentLeg = MkSegmentLeg { slLeg :: !LegPeriod
                               , slDEIs :: [SegmentDEI]
                               } deriving (Show)

-- | A segment as a sequence of legs periods.
type SegmentPeriod = [SegmentLeg]

spFlight :: SegmentPeriod -> Flight
spFlight = lpFlight . slLeg . head

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

spRestriction :: SegmentPeriod -> RestrictService
spRestriction s = if n <= mkLegSequence 11
    then let r = lpRestrictionAt n . slLeg $ head s
         in if r /= ExtRestriction
              then mkRestrictAll r
              else overflow
    else overflow
  where n = lpSequence . slLeg $ last s
        overflow = foldMap merge (slDEIs $ head s)
        merge (MkDEI17x r) = r
        merge _            = mkRestrictNone
