module Journey.Connection (
      fromSegments
    , toOnDs
    , connectionsDate
    , connectionsPeriod
    , OnDSegments
    ) where

import Data.Monoid (mconcat, First(..), getFirst)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldMap)
import Control.Monad (mzero)
import Data.Time.Calendar (Day, addDays, diffDays)
import Data.Time.Clock (secondsToDiffTime)

import qualified Journey.EnumMap as M
import Journey.Types

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
connectionsDate :: OnDSegments -> Day -> Path -> [[SegmentDate]]
connectionsDate onds d0 = map (($[]) . fst) . foldl combine s0 . toSteps
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
            case connectDate d t cmin cmax outgoing of
              Nothing     -> mzero
              Just (o, w) -> let elapsed = count (spElapsedTime outgoing) w
                                 timeleft' = timeleft - elapsed
                             in return $ (done . (o:), (Just o, timeleft'))

-- | Feasible connections on periods.
connectionsPeriod :: OnDSegments -> Path -> [[SegmentPeriod]]
connectionsPeriod onds = map extract . foldl combine acc0 . toSteps
  where acc0 = [(id, (Nothing, secondsToDiffTime 24*60*60, 0))]
        combine acc (a, b) = do
            (done, (incoming, timeleft, dvar)) <- acc
            outgoing <- M.find (MkPOnD a b) onds
            let (p, t, d, cmin, cmax, count) = case incoming of
                  Nothing -> ( maxPeriod
                             , secondsToDiffTime 0
                             , 0
                             , secondsToDiffTime 0
                             , timeleft
                             , const )
                  Just i  -> ( spPeriod i
                             , spArrivalTime i
                             , spArrivalDateVariation i
                             , mct i outgoing
                             , min timeleft $ secondsToDiffTime 6*60*60
                             , (+) )
            case connectPeriod p t d cmin cmax outgoing of
              Nothing         -> mzero
              Just (o, w, d') -> let elapsed = count (spElapsedTime outgoing) w
                                     timeleft' = timeleft - elapsed
                                     e = (o, d')
                                 in return $ (done . (e:), (Just o, timeleft', d'))
        extract (dlist, (Just s', _, d')) = map (restrict d') $ dlist []
        restrict d' (s, d) = let p = shiftPeriod (d - d') (spPeriod s)
                                in alterPeriod p s

-- | Try to connect an onward segment.
connectDate :: Day
            -> ScheduleTime
            -> TimeDuration
            -> TimeDuration
            -> SegmentPeriod
            -> Maybe (SegmentDate, TimeDuration)
connectDate d0 t0 cmin cmax seg = case find (match . fst) $ zip dates waits of
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

-- | Try to connect an onward segment.
connectPeriod :: Period
              -> ScheduleTime
              -> Int
              -> TimeDuration
              -> TimeDuration
              -> SegmentPeriod
              -> Maybe (SegmentPeriod, TimeDuration, Int)
connectPeriod p0 t0 d0 cmin cmax s = getFirst $ foldMap match (zip dates waits)
  where match (d, w) = let d' = d0 + d
                           p' = shiftPeriod d' p0
                       in case intersectPeriods p' (spPeriod s) of
                         Nothing -> First Nothing
                         Just p  -> First $ Just (alterPeriod p s, w, d')
        dates = [0..] :: [Int]
        waits = takeWhile (<cmax) . dropWhile (<cmin) $ map wait dates
        wait d = t - t0 + secondsToDiffTime (fromIntegral d * 86400)
        t = spDepartureTime s

