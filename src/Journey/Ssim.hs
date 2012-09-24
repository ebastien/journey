{-# LANGUAGE OverloadedStrings #-}

module Journey.Ssim (
      readSsimFile
    , ssimSegments
    ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lex.Integral (readDecimal)
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP
import Data.Functor ((<$>))
import Control.Monad (void, join)
import Control.Applicative (pure, some, (<*>), (<*), (*>), (<|>))
import Data.List (groupBy, elemIndex)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Char (ord, chr)
import Data.Bits (bit)
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.LocalTime (timeOfDayToTime, makeTimeOfDayValid)

import Journey.Types ( AirlineCode(..), LegPeriod(..), SegmentPeriod, SegmentDEI
                     , segmentIdx, SegmentLeg(..), Flight(..), Port(..), Dow(..)
                     , PeriodBoundary, TimeVariation, ScheduleTime, OnD)

import Journey.Parsers

{-------------------------------------------------------------------------------
  SSIM data types
-------------------------------------------------------------------------------}

data Header = Header deriving Show

data Carrier = Carrier { cAirline :: !AirlineCode 
                       } deriving Show

-- | Segment data record.
data SegmentData = SegmentData { dFlight :: Flight
                               , dIndex :: !Int
                               , dBoard :: !Port
                               , dOff :: !Port
                               , dDEI :: !SegmentDEI
                               } deriving Show

data LegGroup = LegGroup { lgLeg :: LegPeriod
                         , lgSegments :: [SegmentData] } deriving Show

type FlightGroup = [LegGroup]

data CarrierGroup = CarrierGroup { cgCarrier :: Carrier
                                 , cgFlights :: [FlightGroup] } deriving Show

data Ssim = Ssim { ssimHeader :: Header
                 , ssimCarriers :: [CarrierGroup] } deriving Show

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

-- | Parser for header records.
headerP :: Parser Header
headerP  = P.char '1' >> P.take 199 >> (some P.endOfLine) >> return Header

-- | Parser for carrier records.
carrierP :: Parser Carrier
carrierP = do
  void $ P.char '2'
  void $ P.satisfy (\c -> c == 'U' || c == 'L')
  airline <- airlineP
  void $ P.take 195
  void $ some P.endOfLine
  return (Carrier airline)

-- | Parser for leg records.
legPeriodP :: Parser LegPeriod
legPeriodP = do
  void (P.char '3')         <?> "Leg record type"
  suffix <- P.anyChar
  airline <- airlineP
  fnum <- fnumP             <?> "Leg flight number"
  iviL <- decimalP 2        <?> "Leg itinerary variation identifier (low)"
  lsn <- decimalP 2         <?> "Leg sequence number"
  void $ P.anyChar
  bdate <- dateP            <?> "Leg period of operation (begin)"
  edate <- periodBoundaryP  <?> "Leg period of operation (end)"
  dow <- dowP               <?> "Leg days of week"
  void $ P.anyChar
  bpoint <- portP           <?> "Leg board point"
  void $ P.take 4
  dtime <- scheduleTimeP    <?> "Leg departure time"
  dtvar <- timeVariationP   <?> "Leg departure time variation"
  void $ P.take 2
  opoint <- portP           <?> "Leg off point"
  atime <- scheduleTimeP    <?> "Leg arrival time"
  void $ P.take 4
  atvar <- timeVariationP   <?> "Leg arrival time variation"
  void $ P.take (2+3+20+5+10+9+2+6)
  iviH <- paddedDecimalP 1  <?> "Leg itinerary variation identifier (high)"
  void $ P.take (3+3+3+9+1+1+1+11+1+11+20)
  ddvar <- dateVariationP   <?> "Leg departure date variation"
  advar <- dateVariationP   <?> "Leg arrival date variation"
  void $ P.take 6
  void $ some P.endOfLine
  let variation = iviL + 100 * iviH
      flight = Flight airline fnum suffix variation
      period = (bdate, edate, dow)
      etime = (atime - atvar) - (dtime - dtvar)
            + (secondsToDiffTime . fromIntegral $ (advar - ddvar) * 86400)
  return $ LegPeriod flight period lsn
                     bpoint opoint
                     dtime atime etime ddvar advar

-- | Parser for segment records.
segmentP :: Parser SegmentData
segmentP = do
  void (P.char '4')         <?> "Segment record type"
  suffix <- P.anyChar       <?> "Segment operational suffix"
  airline <- airlineP       <?> "Segment airline code"
  fnum <- fnumP             <?> "Segment flight number"
  iviL <- decimalP 2        <?> "Segment itinerary variation identifier (low)"
  _lsn <- decimalP 2        <?> "Segment leg sequence number"
  void $ P.anyChar
  void $ P.take 13
  iviH <- paddedDecimalP 1  <?> "Segment itinerary variation identifier (high)"
  idx <- pointsIndicatorP   <?> "Segment points indicator"
  dei <- decimalP 3         <?> "Segment DEI"
  bpoint <- portP           <?> "Segment board point"
  opoint <- portP           <?> "Segment off point"
  void $ P.take 155
  void $ P.take 6
  void $ some P.endOfLine
  let variation = iviL + 100 * iviH
      flight = Flight airline fnum suffix variation
  return $ SegmentData flight idx bpoint opoint dei

-- | Parser for trailer records.
trailerP :: Parser ()
trailerP = (P.char '5'       <?> "Trailer record type")
        *> (P.take 199       <?> "Trailer padding")
        *> (some P.endOfLine <?> "Trailer separator")
        *> pure ()

-- | Parser for leg groups.
legGroupP :: Parser LegGroup
legGroupP = LegGroup <$> (legPeriodP    <?> "Leg record")
                     <*> (some segmentP <?> "Leg segments")

-- | Parser for carrier groups.
carrierGroupP :: Parser CarrierGroup
carrierGroupP = CarrierGroup <$> (carrierP               <?> "Carrier record")
                             <*> (grp <$> some legGroupP <?> "Carrier legs")
                             <*  (trailerP               <?> "Carrier trailer")
  where grp = groupBy ((==) `on` lpFlight . lgLeg)

-- | Parser for SSIM file.
ssimP :: Parser Ssim
ssimP = Ssim <$> (headerP            <?> "SSIM7 header")
             <*> (some carrierGroupP <?> "SSIM7 carriers")

{-------------------------------------------------------------------------------
  Interface
-------------------------------------------------------------------------------}

-- | Run the SSIM parser on the given file.
readSsimFile :: String -> IO Ssim
readSsimFile s = do
  result <- LP.parse ssimP <$> LB.readFile s
  case result of
    LP.Fail left ctx msg -> fail . unlines $ msg:(show $ LB.length left):ctx
    LP.Done _ ssim      -> return ssim

-- | Extract segments from a flight
flightSegments :: FlightGroup -> [(OnD, SegmentPeriod)]
flightSegments = join . combine
  where combine []         = []
        combine xs@(x:xs') = [ mkAssoc y | y <- xs ] : combine xs'
          where legX = lgLeg x
                mkAssoc y = ((lpBoard legX, lpOff legY), map mkLeg legs)
                  where legY = lgLeg y
                        legs = takeWhile (on (>=) (lpSequence . lgLeg) $ y) xs
                        mkLeg l = MkSegmentLeg legL $ map dDEI . filter ((==) idx . dIndex) $ segL
                          where segL = lgSegments l
                                legL = lgLeg l
                                idx = segmentIdx (lpSequence legL) (lpSequence legY)

-- | Extract segments from a SSIM structure.
ssimSegments :: Ssim -> [(OnD, SegmentPeriod)]
ssimSegments ssim = do
  car <- ssimCarriers ssim
  flt <- cgFlights car
  flightSegments flt
