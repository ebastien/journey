module Journey.Connection (
      connectionsPeriod
    ) where

import Data.Functor ((<$>))
import Data.Monoid (mconcat, First(..), getFirst)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Foldable (foldMap)
import Control.Monad (mzero, guard)
import Data.Time.Calendar (Day, addDays, diffDays)
import Data.Time.Clock (secondsToDiffTime)

import Journey.Types
import Journey.Period
import Journey.Flight
import Journey.SegmentPeriod
import Journey.Restriction
import Journey.MCT.Rule

{-------------------------------------------------------------------------------
  Connection building
-------------------------------------------------------------------------------}

-- | Convert a path into a list of OnDs.
toSteps :: Path -> [OnD]
toSteps path = zip path $ tail path

-- | Minimal support for MCT check.
isMCT :: (Rule -> Maybe MCT) -> SegmentPeriod -> SegmentPeriod -> Bool
isMCT regn a b = isJust $ secondsToDiffTime . (*60) . fromIntegral . getMCT <$> regn q
  where q = mkQuery ct $ defaultOptions ip tf
        ip = spOff a == spBoard b
        t0 = spArrivalTime a
        t1 = spDepartureTime b
        ct = fromTimes t0 t1
        tf = MkTransitFlow (spArrivalArea a) (spDepartureArea b)

notSameFlight :: Maybe SegmentPeriod -> SegmentPeriod -> Bool
notSameFlight i o = case i of
  Nothing -> True
  Just s  -> spFlight s /= spFlight o

toLocal :: (Port -> Country) -> SegmentPeriod -> Local AirlineCode
toLocal m s = Local q $ Transfer c i r
  where c = fAirline $ spFlight s
        i = m (spBoard s) /= m (spOff s)
        r = rPax $ spRestrictService s
        q = spRestrictQualifier s

-- | Feasible connections on periods.
connectionsPeriod :: (OnD -> [SegmentPeriod]) -- ^ Segment lookup by OnD
                  -> (Port -> Country)        -- ^ Country lookup by port
                  -> (Rule -> Maybe MCT)      -- ^ MCT lookup
                  -> Path                     -- ^ Path to connect
                  -> [[SegmentPeriod]]
connectionsPeriod segs geos regn = extract . foldl combine acc0 . toSteps
  where acc0 = [(id, Nothing, secondsToDiffTime 24*60*60, 0, Nothing)]
        combine acc ond = do
            (done, incoming, timeleft, dvar, tr) <- acc
            outgoing <- segs ond
            guard $ notSameFlight incoming outgoing
            
            let l = toLocal geos outgoing
                tr' = case tr of
                  Nothing -> initiate l
                  Just x  -> connect l x
            guard $ isJust tr'
            
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
                             , secondsToDiffTime 20*60
                             , min timeleft $ secondsToDiffTime 6*60*60
                             , (+) )
            
            case connectPeriod p t d cmin cmax outgoing of
              Nothing         -> mzero
              Just (o, w, d') ->
                let elapsed = count (spElapsedTime outgoing) w
                    timeleft' = timeleft - elapsed
                    e = (o, d')
                in return $ (done . (e:), Just o, timeleft', d', tr')
        
        extract trips = do
          (dlist, s', _, d', tr) <- trips
          guard . isJust $ tr >>= finalize
          let p' = spPeriod $ fromJust s'
              xs = dlist []
              ys = map fst xs
          guard . all (uncurry $ isMCT regn) $ zip ys (tail ys)
          return $ map (restrict p' d') xs
        
        restrict p' d' (s, d) = alterPeriod p s
          where p = shiftPeriod (d - d') p'

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

