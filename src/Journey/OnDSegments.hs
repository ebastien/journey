module Journey.OnDSegments (
      fromSegments
    , toOnDs
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
toOnDs = map unpackOnd . M.keys
  where unpackOnd (MkPOnD a b) = (a,b)

fromOnD :: OnDSegments -> OnD -> [SegmentPeriod]
fromOnD segs (a, b) = M.find (MkPOnD a b) segs
