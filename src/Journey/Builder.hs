{-# LANGUAGE OverloadedStrings #-}

module Journey.Builder (
    buildAll
  , buildSplit
  , buildPathPeriod
  , buildForOnD
  , buildAllPaths
  ) where

import Data.List (sort, group, intersperse)
import Data.Monoid (mconcat, mempty, mappend)
import Data.Foldable (foldMap)
import Data.Functor ((<$>))
import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Data.Text.Format (build, left, Shown(..))
import qualified Data.Text.Buildable as TB
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import Data.Time.Calendar (Day, toGregorian)

import Journey.Types
import Journey.Period
import Journey.Flight
import Journey.SegmentDate
import Journey.SegmentPeriod
import Journey.LegPeriod
import Journey.Route

type PathBuilder = Path -> (Builder -> Builder) -> Builder

buildPathPeriod :: (Path -> [[SegmentPeriod]]) -> PathBuilder
buildPathPeriod cnx pth bld = foldMap (bld . buildCnxPeriod) $ cnx pth

-- | Build a representation of all itineraries.
buildAll :: (MetricSpace e) => [PortCoverages e]
                            -> PathBuilder
                            -> Builder
buildAll covs bld = foldMap (buildForOnD covs bld) onds
  where onds = map head . group . sort $ concatMap coveredOnDs covs

-- | Build a representation of all itineraries in chunks.
buildSplit :: (MetricSpace e) => [PortCoverages e]
                             -> PathBuilder
                             -> Int
                             -> [Builder]
buildSplit covs bld n = foldMap (buildForOnD covs bld) <$> chunk n onds
  where onds = map head . group . sort $ concatMap coveredOnDs covs

-- | Split a list into chunks.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs

-- | Build a representation of itineraties for a single OnD.
buildForOnD :: (MetricSpace e) => [PortCoverages e]
                               -> PathBuilder
                               -> OnD
                               -> Builder
buildForOnD covs bld ond = foldMap f covs
  where f cov = case ondPaths ond cov of
                  Just paths -> foldMap (buildForPath ond bld) paths
                  Nothing    -> mempty

buildAllPaths :: (MetricSpace e) => [PortCoverages e]
                                 -> Builder
buildAllPaths covs = foldMap f onds
  where f ond = foldMap (g ond) covs
        g ond cov = case ondPaths ond cov of
          Just ps -> foldMap (\p -> buildPath p `mappend` singleton '\n') ps
          Nothing -> mempty
        onds = map head . group . sort $ concatMap coveredOnDs covs

-- | Build a representation of itineraties for a single path.
buildForPath :: OnD -> PathBuilder -> Path -> Builder
buildForPath ond bld path = bld path fmt
  where fmt b = mconcat [ buildOnD ond   , singleton '\t'
                        , buildPath path , singleton '\t'
                        , b              , singleton '\n' ]

-- | Build a representation of an OnD.
buildOnD :: OnD -> Builder
buildOnD (org,dst) = buildPath [org,dst]

-- | Build a representation of a path.
buildPath :: Path -> Builder
buildPath = mconcat . intersperse (singleton '-') . map buildPort

-- | Build a representation of a port.
buildPort :: Port -> Builder
buildPort = fromString . show

-- | Build a representation of a connection of segment-dates.
buildCnxDate :: [SegmentDate] -> Builder
buildCnxDate = buildCnx buildSegmentDate

-- | Build a representation of a connection of segment-periods.
buildCnxPeriod :: [SegmentPeriod] -> Builder
buildCnxPeriod s = mconcat . intersperse (singleton '\t')
                 $ [ buildCnx buildSegmentPeriod s
                   , buildStops $ cxStops s
                   , buildCarriers $ cxCarriers s
                   , buildElapsed $ cxElapsedTime s
                   ]

buildCnx :: (a -> Builder) -> [a] -> Builder
buildCnx b = mconcat . intersperse (singleton ';') . map b

-- | Build a representation of a segment-date.
buildSegmentDate :: SegmentDate -> Builder
buildSegmentDate s = mconcat . intersperse (singleton ' ')
                   $ [ buildFlight . lpFlight . slLeg . head $ sdSegment s
                     , buildPort $ sdBoard s
                     , buildPort $ sdOff s
                     , buildDate $ sdDepartureDate s
                     , buildTime $ sdDepartureTime s
                     , buildDate $ sdArrivalDate s
                     , buildTime $ sdArrivalTime s
                     ]

-- | Build a representation of a segment-period.
buildSegmentPeriod :: SegmentPeriod -> Builder
buildSegmentPeriod s = mconcat . intersperse (singleton ' ')
                     $ [ buildFlight . lpFlight . slLeg $ head s
                       , buildPort $ spBoard s
                       , buildPort $ spOff s
                       , buildPeriod $ spPeriod s
                       , buildTime $ spDepartureTime s
                       , buildTime $ spArrivalTime s
                       , buildDateVariation $ spArrivalDateVariation s
                       , buildOperating $ spOperating s
                       ]

-- | Build a representation of a flight.
buildFlight :: Flight -> Builder
buildFlight f = build "{} {}" (Shown $ fAirline f, left 5 ' ' $ fNumber f)

buildPeriod :: Period -> Builder
buildPeriod (b,e,w) = build "{}-{}/{}" (buildDate b, buildDate e, buildDow w)

buildDow :: Dow -> Builder
buildDow = fromString . show

buildDateVariation :: Int -> Builder
buildDateVariation = left 2 ' '

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

buildStops :: Int -> Builder
buildStops = left 2 ' '

buildCarriers :: [AirlineCode] -> Builder
buildCarriers = mconcat . intersperse (singleton '-') . map buildCarrier

buildCarrier :: AirlineCode -> Builder
buildCarrier = fromString . show

buildElapsed :: TimeDuration -> Builder
buildElapsed t = left 5 ' ' (truncate t :: Int)

buildOperating :: Bool -> Builder
buildOperating o = TB.build $ case o of
                     True  -> 'O'
                     False -> 'M'
