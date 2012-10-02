module Journey.SegmentDate (
    SegmentDate(..)
  , sdBoard
  , sdOff
  , sdDepartureTime
  , sdArrivalDate
  , sdArrivalTime
  ) where

import Data.Time.Calendar (Day, addDays)

import Journey.Types
import Journey.SegmentPeriod

-- | A segment date.
data SegmentDate = MkSegmentDate { sdSegment :: SegmentPeriod
                                 , sdDepartureDate :: Day
                                 } deriving (Show)

sdBoard :: SegmentDate -> Port
sdBoard = spBoard . sdSegment

sdOff :: SegmentDate -> Port
sdOff = spOff . sdSegment

sdDepartureTime :: SegmentDate -> ScheduleTime
sdDepartureTime = spDepartureTime . sdSegment

sdArrivalDate :: SegmentDate -> Day
sdArrivalDate s = addDays (fromIntegral . spArrivalDateVariation $ sdSegment s)
                $ sdDepartureDate s

sdArrivalTime :: SegmentDate -> ScheduleTime
sdArrivalTime = spArrivalTime . sdSegment
