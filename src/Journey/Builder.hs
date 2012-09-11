{-# LANGUAGE OverloadedStrings #-}

module Journey.Builder (
    buildAll
    ) where

import Data.List (sort, group, intersperse)
import Data.Monoid (mconcat, mempty)
import Data.Foldable (foldMap)
import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Data.Text.Format (build, left, Shown(..))
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import Data.Time.Calendar (Day, toGregorian)

import Journey.Types ( Port, OnD, Path, ScheduleTime, SegmentPeriod
                     , Flight(..), LegPeriod(..)
                     , SegmentDate(..), sdDepartureTime, sdArrivalDate, sdArrivalTime
                     , SegmentLeg(..) )
import Journey.Route ( coveredOnDs, ondPaths, MetricSpace, PortCoverages )
import Journey.Connection ( connections, OnDSegments )

-- | Build a representation of all itineraries departing on a given day.
buildAll :: (MetricSpace e) => OnDSegments
                            -> [PortCoverages e]
                            -> Day
                            -> Builder
buildAll segs covs date = foldMap (buildForOnD segs covs date) onds
  where onds = map head . group . sort $ concatMap coveredOnDs covs

-- | Build a representation of itineraties for a single OnD.
buildForOnD :: (MetricSpace e) => OnDSegments
                               -> [PortCoverages e]
                               -> Day
                               -> OnD
                               -> Builder
buildForOnD segs covs date ond = foldMap f covs
  where f cov = case ondPaths ond cov of
                  Just paths -> foldMap (buildForPath date segs ond) paths
                  Nothing    -> mempty

-- | Build a representation of itineraties for a single path.
buildForPath :: Day -> OnDSegments -> OnD -> Path -> Builder
buildForPath date segs ond path = foldMap f $ connections segs date path
  where f c = mconcat [ buildOnD ond   , singleton '\t'
                      , buildPath path , singleton '\t'
                      , buildCnx c
                      , singleton '\n']

-- | Build a representation of an OnD.
buildOnD :: OnD -> Builder
buildOnD (org,dst) = buildPath [org,dst]

-- | Build a representation of a path.
buildPath :: Path -> Builder
buildPath = mconcat . intersperse (singleton '-') . map buildPort

-- | Build a representation of a port.
buildPort :: Port -> Builder
buildPort = fromString . show

-- | Build a representation of a connection.
buildCnx :: [SegmentDate] -> Builder
buildCnx = mconcat . intersperse (singleton ';') . map buildSeg

-- | Build a representation of a segment.
buildSeg :: SegmentDate -> Builder
buildSeg s = mconcat . intersperse (singleton ' ')
           $ [ buildFlight f
             , buildPort $ lpBoard l
             , buildPort $ lpOff l
             , buildDate $ sdDepartureDate s
             , buildTime $ sdDepartureTime s
             , buildDate $ sdArrivalDate s
             , buildTime $ sdArrivalTime s
             ]
  where l = slLeg . head $ sdSegment s
        f = lpFlight l

-- | Build a representation of a flight.
buildFlight :: Flight -> Builder
buildFlight f = build "{} {}" (Shown $ fAirline f, left 5 ' ' $ fNumber f)

-- | Build a representation of a date.
buildDate :: Day -> Builder
buildDate date = mconcat [pad 4 y, pad 2 m, pad 2 d]
  where (y,m,d) = toGregorian date
        pad n = left n '0'

-- | Build a representation of a time.
buildTime :: ScheduleTime -> Builder
buildTime t = build "{}:{}" [pad h, pad m]
  where TimeOfDay h m _ = timeToTimeOfDay t
        pad = left 2 '0'
