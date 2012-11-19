module Journey.OnDSegments (
      fromSegments
    , toOnDs
    , toOnDPaths
    , OnDSegments
    , fromOnD
    ) where

import qualified Journey.EnumMap as M

import Journey.Types
import Journey.SegmentPeriod

-- | A collection of OnD associations.
type OnDMap a = M.EnumMap POnD a

-- | A collection of OnD and segments associations.
type OnDSegments = OnDMap [SegmentPeriod]

-- | Create the collection of segments grouped by OnD.
fromSegments :: [(OnD, SegmentPeriod)] -> OnDSegments
fromSegments = M.group . map packOnd
  where packOnd ((org,dst), segment) = (MkPOnD org dst, segment)

-- | List unique OnDs.
toOnDs :: OnDSegments -> [OnD]
toOnDs = map unpackOnD . M.keys

-- | List of OnDs and paths.
toOnDPaths :: OnDSegments -> [(OnD, [Path])]
toOnDPaths = map p . M.toList
  where p (ond, segs) = (unpackOnD ond, map spPath segs)

-- | Find segments matching a given OnD.
fromOnD :: OnDSegments -> OnD -> [SegmentPeriod]
fromOnD segs (a, b) = M.find (MkPOnD a b) segs
