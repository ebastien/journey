{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor ((<$>))
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.ByteString.Char8 (pack)
import Data.List (nub, intersperse)
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import Data.Time.Calendar (Day, toGregorian)

import Types (
    Port
  , OnD
  , Path
  , ScheduleTime
  , toPort
  , toDate
  )
import Ssim (
    readSsimFile
  , ssimSegments
  , Flight(..)
  , LegPeriod(..)
  , SegmentDate(..)
  )
import Journey (
    coverages
  , coveredPaths
  , coveredOnDs
  , ondPaths
  , MetricSpace
  , PortCoverages
  )
import GeoCoord (
    loadReferences
  , assocToCities
  , adjacency
  )
import Connection (
    fromSegments
  , toOnDs
  , connections
  , OnDSegments
  )

import Data.Monoid (mconcat, mempty, mappend)
import Data.Foldable (foldMap)
import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText, singleton)
import Data.Text.Format (build, left, Shown(..))

buildAll :: (MetricSpace e) => OnDSegments
                            -> [PortCoverages e]
                            -> Day
                            -> Builder
buildAll segs covs date = foldMap (buildForOnD segs covs date) onds
  where onds = nub $ concatMap coveredOnDs covs

buildForOnD :: (MetricSpace e) => OnDSegments
                               -> [PortCoverages e]
                               -> Day
                               -> OnD
                               -> Builder
buildForOnD segs covs date ond = foldMap build covs
  where build cov = case ondPaths ond cov of
                      Just paths -> foldMap (buildForPath date segs ond) paths
                      Nothing    -> mempty

buildForPath :: Day -> OnDSegments -> OnD -> Path -> Builder
buildForPath date segs ond path = foldMap build $ connections segs date path
  where build c = mconcat [ buildOnD ond   , singleton '\t'
                          , buildPath path , singleton '\t'
                          , buildCnx c
                          , singleton '\n']

buildOnD :: OnD -> Builder
buildOnD (org,dst) = buildPath [org,dst]

buildPath :: Path -> Builder
buildPath = mconcat . intersperse (singleton '-') . map buildPort

buildPort :: Port -> Builder
buildPort = fromString . show

buildCnx :: [SegmentDate] -> Builder
buildCnx = mconcat . intersperse (singleton ';') . map buildSeg

buildSeg :: SegmentDate -> Builder
buildSeg s = mconcat . intersperse (singleton ' ') $ [ buildFlight f
                                                     , buildPort $ lpBoard l
                                                     , buildPort $ lpOff l
                                                     , buildDate $ sdDepartureDate s
                                                     , buildTime $ sdDepartureTime s
                                                     , buildDate $ sdArrivalDate s
                                                     , buildTime $ sdArrivalTime s
                                                     ]
  where l = fst . head $ sdSegment s
        f = lpFlight l

buildFlight :: Flight -> Builder
buildFlight f = build "{} {}" (Shown $ fAirline f, left 5 ' ' $ fNumber f)

buildDate :: Day -> Builder
buildDate date = mconcat [pad 4 y, pad 2 m, pad 2 d]
  where (y,m,d) = toGregorian date
        pad n = left n '0'

buildTime :: ScheduleTime -> Builder
buildTime t = build "{}:{}" [pad h, pad m]
  where TimeOfDay h m _ = timeToTimeOfDay t
        pad = left 2 '0'

-- | 
main :: IO ()
main = do
  [refsFile, ssimFile, beginDate, endDate] <- getArgs
  refs <- loadReferences refsFile
  segments <- fromSegments . assocToCities refs . ssimSegments <$> readSsimFile ssimFile

  let covs = take 3 . coverages . adjacency refs $ toOnDs segments
      dateL = fromJust . toDate $ pack beginDate
      dateH = fromJust . toDate $ pack endDate

  -- let (a,b) = join (***) (fromJust . toPort) ("NYC", "STL")
  -- mapM_ (putStrLn . show) $ M.find (MkPOnD a b) segments
  -- T.putStr . toLazyText . buildForOnD segments covs dateL $ join (***) (fromJust . toPort) ("LON", "DFW")

  T.putStr . toLazyText $ foldMap (buildAll segments covs) [dateL..dateH]
