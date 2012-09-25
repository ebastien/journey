{-# LANGUAGE OverloadedStrings #-}

module Journey.Parsers (
      paddedDecimalP
    , decimalP
    , airlineP, toAirlineCode
    , portP, toPort
    , dowP, toDow
    , dateP, toDate
    , periodBoundaryP, toPeriodBoundary
    , timeVariationP
    , dateVariationP
    , scheduleTimeP
    , fnumP
    , pointsIndicatorP
    , packWith
    , packBoundedWith
    , alphaPack
    , alphaNumPack
    ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lex.Integral (readDecimal)
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP
import Data.Functor ((<$>))
import Control.Monad (void, join)
import Control.Applicative (pure, some, (<*>), (<*), (*>), (<|>), liftA2)
import Data.List (elemIndex)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Char (ord, chr)
import Data.Bits (bit)
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.LocalTime (timeOfDayToTime, makeTimeOfDayValid)

import Journey.Types

-- | Parser for fixed length decimal numbers
-- with space padding and defaulting to zero.
paddedDecimalP :: Int -> Parser Int
paddedDecimalP n = do
  s <- B8.dropWhile (== ' ') <$> P.take n
  fromMaybe (fail ("Decimal parsing failed on " ++ show s))
          $ (return . fst) <$> if B8.null s
                                 then Just (0, B8.empty)
                                 else readDecimal s

-- | Parser for fixed length decimal numbers.
decimalP :: Int -> Parser Int
decimalP n = do
  i <- readDecimal <$> P.take n
  fromMaybe (fail "Decimal parsing failed") $ (return . fst) <$> i

-- | Parser generator for fixed-length strings packed as numbers.
packWith :: Num a => Int -> (Int -> Parser a) -> Parser a
packWith n f = sum <$> (mapM f $ take n [0..])

-- | Parser generator for bounded-length strings packed as numbers.
packBoundedWith :: Num a => Int -> Int -> Parser () -> (Int -> Parser a) -> Parser a
packBoundedWith l h end f = sum <$> loop 0
  where loop n | n < l = step n
        loop n | n < h = (end *> pure []) <|> step n
        loop _         = pure []
        step n = liftA2 (:) (f n) (loop $ n+1)

-- | Parser generator for a packed letter.
alphaPack :: Int -> Parser Int
alphaPack n = (* 26^n) . subtract (ord 'A') . ord <$> P.satisfy letter
  where letter c = c >= 'A' && c <= 'Z'

-- | Parser generator for a packed alphanumeric character.
alphaNumPack :: Int -> Parser Int
alphaNumPack n = (* 37^n) <$> letter <|> digit <|> space
  where letter = (+11) . subtract (ord 'A') . ord <$> P.satisfy isUpperLetter
        digit  =  (+1) . subtract (ord '0') . ord <$> P.digit
        space  = pure 0 <* P.char ' '
        isUpperLetter c = c >= 'A' && c <= 'Z'

-- | ByteString parsing to Maybe.
maybeParse :: Parser a -> B8.ByteString -> Maybe a
maybeParse p = either (const Nothing) Just . P.parseOnly p

-- | Parser for airline codes.
airlineP :: Parser AirlineCode
airlineP = MkAirlineCode <$> packWith 3 step <?> "Airline code"
  where step n = (* 37^n) <$> code n
        code n | n < 2     = letter <|> digit
               | otherwise = letter <|> digit <|> space
        letter = (+11) . subtract (ord 'A') . ord <$> P.satisfy isUpperLetter
        digit  =  (+1) . subtract (ord '0') . ord <$> P.digit
        space  = pure 0 <* P.char ' '
        isUpperLetter c = c >= 'A' && c <= 'Z'

-- | Try to convert a ByteString to an AirlineCode.
toAirlineCode :: B8.ByteString -> Maybe AirlineCode
toAirlineCode = maybeParse airlineP

-- | Parser for ports.
portP :: Parser Port
portP = MkPort <$> packWith 3 alphaPack <?> "Port"

-- | Try to convert a ByteString to a Port.
toPort :: B8.ByteString -> Maybe Port
toPort = maybeParse portP

-- | Parser for days of week.
dowP :: Parser Dow
dowP = MkDow <$> packWith 7 step <?>  "Days of week"
  where step n = P.char (chr $ ord '1' + n) *> (pure $ bit n)
             <|> P.char ' ' *> (pure 0)

-- | Try to convert a ByteString to days of the week.
toDow :: B8.ByteString -> Maybe Dow
toDow = maybeParse dowP

-- | Parser for days.
dayP :: Parser Int
dayP = decimalP 2

-- | Parser for months.
monthP :: Parser Int
monthP = do
  m <- (flip elemIndex) months <$> P.take 3
  fromMaybe (fail "Month parsing failed") $ return <$> m
  where months = [ "XXX", "JAN","FEB", "MAR","APR", "MAY",
                   "JUN","JUL","AUG", "SEP", "OCT", "NOV", "DEC" ]

-- | Parser for years.
yearP :: Num a => Parser a
yearP = (fromIntegral . (2000+)) <$> decimalP 2

-- | Parse for dates.
dateP :: Parser Day
dateP = do
  d <- dayP; m <- monthP; y <- yearP
  fromMaybe (fail "Date parsing failed")
          $ return <$> (fromGregorianValid y m d)

-- | Try to convert a ByteString to a date.
toDate :: B8.ByteString -> Maybe Day
toDate = maybeParse dateP

-- | Parser for period boundaries.
periodBoundaryP :: Parser PeriodBoundary
periodBoundaryP = do
  d <- dayP; m <- monthP; y <- yearP
  if d == 0 && m == 0
    then return Nothing
    else fromMaybe (fail "Period boundary parsing failed")
                 $ (return . Just) <$> (fromGregorianValid y m d)

-- | Try to convert a ByteString to a period boundary.
toPeriodBoundary :: B8.ByteString -> Maybe PeriodBoundary
toPeriodBoundary = maybeParse periodBoundaryP

-- | Parser for time variations.
timeVariationP :: Parser TimeVariation
timeVariationP = (plus <|> minus) <*> time
  where plus = P.char '+' *> pure id
        minus = P.char '-' *> pure negate
        time = do
          h <- decimalP 2; m <- decimalP 2
          if h <= 23 && m <= 59
            then return . secondsToDiffTime . fromIntegral $ 60 * h + m
            else fail "Time variation parsing failed"

-- | Parser for date variations.
dateVariationP :: Parser Int
dateVariationP = fromIntegral <$> (before <|> after)
  where before = pure (-1) <* P.char 'J'
        after  = subtract (ord '0') . ord <$> P.digit

-- | Parser for schedule times.
scheduleTimeP :: Parser ScheduleTime
scheduleTimeP = do
  m <- makeTimeOfDayValid <$> decimalP 2 <*> decimalP 2 <*> pure 0
  fromMaybe (fail "Local time parsing failed") $ return . timeOfDayToTime <$> m

-- | Parser for flight numbers.
fnumP :: Parser Int
fnumP = paddedDecimalP 4 <?> "Flight number"

-- | Parser for board and off points indicators.
pointsIndicatorP :: Parser Int
pointsIndicatorP = packWith 2 alphaPack <?> "Board and off points indicator"
