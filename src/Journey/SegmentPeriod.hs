module Journey.SegmentPeriod (
    SegmentLeg(..)
  , SegmentPeriod
  , SegmentDataElement(..)
  , spFlight
  , spPeriod
  , alterPeriod
  , spBoard, spOff
  , spDepartureTime
  , spArrivalTime
  , spArrivalDateVariation
  , spElapsedTime
  , spRestrictService
  , spRestrictQualifier
  , spDepartureArea
  , spArrivalArea
  , spPath
  , spStops
  , cxStops
  , cxCarriers
  , cxElapsedTime
  ) where

import Data.Foldable (foldMap)
import Data.Monoid (First(..))
import Data.Time.Clock (secondsToDiffTime)
import Data.Maybe (fromMaybe)

import Journey.Types
import Journey.Period
import Journey.Flight
import Journey.LegPeriod
import Journey.Restriction

-- | Segment data element.
data SegmentDataElement = IgnoredElement
                        | MkDEI108
                        | MkDEI003
                        | MkDEI113
                        | MkDEI121
                        | MkDEI104
                        | MkDEI005
                        | MkDEI115
                        | MkDEI004
                        | MkDEI114
                        | MkDEI127
                        | MkDEI002
                        | MkDEI009
                        | MkDEI8xx
                        | MkDEI9xx
                        | MkDEI010
                        | MkDEI050
                        | MkDEI505
                        | MkDEI303
                        | MkDEI301
                        | MkDEI302
                        | MkDEI122
                        | MkDEI503
                        | MkDEI001
                        | MkDEI125
                        | MkDEI007
                        | MkDEI109
                        | MkDEI111
                        | MkDEI220
                        | MkDEI501
                        | MkDEI006
                        | MkDEI011
                        | MkDEI299
                        | MkDEI106
                        | MkDEI101
                        | MkDEI107
                        | MkDEI102
                        | MkDEI198
                        | MkDEI199
                        | MkDEI210
                        | MkDEI507
                        | MkDEI105
                        | MkDEI201
                        | MkDEI17x !RestrictService
                        | MkDEI713_799
                        | MkDEI71x !Bool !Bool
                        deriving (Show, Eq)

-- | The projection of a leg to a specific segment.
data SegmentLeg = MkSegmentLeg { slLeg :: !LegPeriod
                               , slDEs :: [SegmentDataElement]
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

spRestrictService :: SegmentPeriod -> RestrictService
spRestrictService s = if n <= mkLegSequence 11
    then case lpRestrictionAt n . slLeg $ head s of
           Just r  -> mkRestrictAll r
           Nothing -> overflow
    else overflow
  where n = lpSequence . slLeg $ last s
        overflow = foldMap merge (slDEs $ head s)
        merge (MkDEI17x r) = r
        merge _            = mkRestrictNone

spRestrictQualifier :: SegmentPeriod -> RestrictQualifier
spRestrictQualifier s = fromMaybe (False, False) q
  where q = getFirst $ foldMap (First . merge) (slDEs $ head s)
        merge (MkDEI71x q p) = Just (q, p)
        merge _              = Nothing

spDepartureArea :: SegmentPeriod -> TransitArea
spDepartureArea = lpDepartureArea . slLeg . head

spArrivalArea :: SegmentPeriod -> TransitArea
spArrivalArea = lpArrivalArea . slLeg . last

spPath :: SegmentPeriod -> Path
spPath s = board : map (lpOff . slLeg) s
  where board = lpBoard . slLeg $ head s

-- | Number of stops for a segment.
spStops :: SegmentPeriod -> Int
spStops s | not $ null s = length s - 1

spOperating :: SegmentPeriod -> Bool
spOperating = undefined

-- | Number of stops for a connection.
cxStops :: [SegmentPeriod] -> Int
cxStops c | not $ null c = sum (map spStops c) + length c - 1

-- | List of carriers for a connection.
cxCarriers :: [SegmentPeriod] -> [AirlineCode]
cxCarriers = map $ fAirline . spFlight

-- | Elapsed time over a connection.
cxElapsedTime :: [SegmentPeriod] -> TimeDuration
cxElapsedTime s = (spElapsedTime $ head s) + (sum . map cnx . zip s $ tail s)
  where cnx (a,b) = spElapsedTime b + case spDepartureTime b - spArrivalTime a of
                      d | d < 0 -> d + secondsToDiffTime (fromIntegral 86400)
                      d         -> d

cxOperating :: [SegmentPeriod] -> Bool
cxOperating = undefined
