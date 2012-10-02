module Journey.Period (
    Dow
  , mkDowValid
  , everyDow
  , Period
  , maxPeriod
  , shiftPeriod
  , intersectPeriods
  , withinPeriod
  ) where

import Data.Char (chr, ord)
import Data.Word (Word8)
import Data.Bits (testBit, shift, (.&.), (.|.))
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

newtype Dow = MkDow Word8

instance Show Dow where
  show (MkDow w) = map step [0..6]
    where step n | testBit w n = chr (n + ord '1')
                 | otherwise   = ' '

mkDowValid :: Word8 -> Dow
mkDowValid d = MkDow $ d .&. 0x7F

-- | Lookup a single day of week.
lookupDow :: Int -> Dow -> Bool
lookupDow n (MkDow w) = testBit w (n-1)

everyDow :: Dow
everyDow = MkDow 0x7F

instance Bounded Day where
  minBound = fromGregorian 1900 1 1
  maxBound = fromGregorian 9999 1 1

shiftDate :: Int -> Day -> Day
shiftDate n d = let d' = addDays (fromIntegral n) d
                in min maxBound $ max minBound d'

type Period = (Day, Day, Dow)

-- | Test if a day is within a period.
withinPeriod :: Period -> Day -> Bool
withinPeriod (l,h,o) d = low && high && dow
  where low = d >= l
        high = d <= h
        dow = let (_, _, n) = toWeekDate d in lookupDow n o

maxPeriod :: Period
maxPeriod = (minBound, maxBound, everyDow)

rotateDow_ :: Int -> Word8 -> Word8
rotateDow_ n d = shift d nL .|. shift d nR
  where nL = n `mod` 7
        nR = negate $ (7 - n) `mod` 7

shiftPeriod :: Int -> Period -> Period
shiftPeriod n (d, e, MkDow w) = (d', e', MkDow w')
  where d' = shiftDate n d
        e' = shiftDate n e
        w' = rotateDow_ n w

intersectPeriods :: Period -> Period -> Maybe Period
intersectPeriods (d1, e1, MkDow w1) (d2, e2, MkDow w2) =
  if d <= e && w /= 0; then Just (d, e, MkDow w); else Nothing
  where d = max d1 d2
        e = min e1 e2
        w = w1 .&. w2
