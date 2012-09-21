module Journey.Connection (
      fromSegments
    , toOnDs
    , connections
    , OnDSegments
    ) where

import Data.Monoid (mconcat, First(..), getFirst)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad (mzero)
import Data.Time.Calendar (Day, addDays, diffDays)
import Data.Time.Clock (secondsToDiffTime)

import qualified Journey.EnumMap as M
import Journey.Types ( Port, SegmentPeriod, spElapsedTime, OnD, POnD(..)
                     , SegmentDate(..), sdArrivalDate, sdArrivalTime, Path
                     , SegmentLeg(..), ScheduleTime, TimeDuration
                     , LegPeriod(..), withinPeriod )

{-------------------------------------------------------------------------------
  Connection building
-------------------------------------------------------------------------------}

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

-- | Convert a path into a list of OnDs.
toSteps :: Path -> [OnD]
toSteps path = zip path $ tail path

-- | Stub for MCT computation.
mct :: SegmentPeriod -> SegmentPeriod -> TimeDuration
mct _ _ = secondsToDiffTime 30*60

-- | Feasible connections on a given day.
connections :: OnDSegments -> Day -> Path -> [[SegmentDate]]
connections onds d0 = map (($[]) . fst) . foldl combine s0 . toSteps
  where s0 = [(id, (Nothing, secondsToDiffTime 24*60*60))]
        combine acc (a, b) = do
            (done, (incoming, timeleft)) <- acc
            outgoing <- M.find (MkPOnD a b) onds
            let (d, t, cmin, cmax, count) = case incoming of
                  Nothing -> ( d0
                             , secondsToDiffTime 0
                             , secondsToDiffTime 0
                             , timeleft
                             , const )
                  Just i  -> ( sdArrivalDate i
                             , sdArrivalTime i
                             , mct (sdSegment i) outgoing
                             , min timeleft $ secondsToDiffTime 6*60*60
                             , (+) )
            case connect d t cmin cmax outgoing of
              Nothing     -> mzero
              Just (o, w) -> let elapsed = count (spElapsedTime outgoing) w
                                 timeleft' = timeleft - elapsed
                             in return $ (done . (o:), (Just o, timeleft'))

-- | Try to connect an onward segment.
connect :: Day
        -> ScheduleTime
        -> TimeDuration
        -> TimeDuration
        -> SegmentPeriod
        -> Maybe (SegmentDate, TimeDuration)
connect d0 t0 cmin cmax seg = case find (match . fst) $ zip dates waits of
                                    Nothing     -> Nothing
                                    Just (d, w) -> Just (mkSeg d, w)
  where legs = map slLeg seg
        firstLeg = head legs
        lastLeg = last legs
        match = withinPeriod (lpPeriod firstLeg)
        dates = [d0..]
        waits = takeWhile (<cmax) . dropWhile (<cmin) $ map wait dates
        wait d = t - t0 + secondsToDiffTime (diffDays d d0 * 86400)
        t = lpDepartureTime firstLeg
        mkSeg = MkSegmentDate seg
