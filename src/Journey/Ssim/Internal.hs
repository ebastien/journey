{-# LANGUAGE OverloadedStrings #-}

module Journey.Ssim.Internal (
    readSsimFile
  , ssimRegularSegments
  , legPeriodP
  , segmentP
  , SegmentData(..)
  ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lex.Integral (readDecimal)
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP
import Data.Functor ((<$>))
import Control.Monad (void, join, guard)
import Control.Applicative (pure, some, (<*>), (<*), (*>), (<|>))
import Data.List (groupBy, elemIndex)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Char (ord, chr)
import Data.Bits (bit)
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.LocalTime (timeOfDayToTime, makeTimeOfDayValid)

import Journey.Types
import Journey.Flight
import Journey.SegmentPeriod
import Journey.LegPeriod
import Journey.Restriction
import Journey.Parsers

{-------------------------------------------------------------------------------
  SSIM data types
-------------------------------------------------------------------------------}

data Header = Header deriving Show

data Carrier = Carrier { cAirline :: !AirlineCode 
                       } deriving Show

newtype SegmentIndex = MkSegmentIndex Int deriving (Show, Eq)

-- | Segment data record.
data SegmentData = SegmentData { dFlight :: Flight
                               , dIndex :: !SegmentIndex
                               , dBoard :: !Port
                               , dOff :: !Port
                               , dElement :: !SegmentDataElement
                               } deriving Show

-- | Segment index from leg sequences.
segmentIndex :: LegSequence -> LegSequence -> SegmentIndex
segmentIndex board off = MkSegmentIndex
                       $ (getSequence off) * 26 + (getSequence board) - 1

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

-- | Parser for optional terminal designator.
optTerminalP :: Parser (Maybe Terminal)
optTerminalP = P.string "  " *> pure Nothing 
           <|> Just <$> terminalP

-- | Parser for leg records.
legPeriodP :: Parser LegPeriod
legPeriodP = do
  void (P.char '3')         <?> "Leg record type"
  suffix <- P.anyChar
  airline <- airlineP
  fnum <- fnumP             <?> "Leg flight number"
  iviL <- decimalP 2        <?> "Leg variation (low)"
  lsn <- mkLegSequence <$> decimalP 2 <?> "Leg sequence number"
  srv <- serviceTypeP       <?> "Leg service type"
  bdate <- dateP            <?> "Leg period of operation (begin)"
  edate <- periodBoundaryP  <?> "Leg period of operation (end)"
  dow <- dowP               <?> "Leg days of week"
  freq <- frequencyRateP    <?> "Leg frequency rate"
  bpoint <- portP           <?> "Leg board point"
  void $ P.take 4
  dtime <- scheduleTimeP    <?> "Leg departure time"
  dtvar <- timeVariationP   <?> "Leg departure time variation"
  dterm <- optTerminalP     <?> "Leg departure terminal"
  opoint <- portP           <?> "Leg off point"
  atime <- scheduleTimeP    <?> "Leg arrival time"
  void $ P.take 4
  atvar <- timeVariationP   <?> "Leg arrival time variation"
  aterm <- optTerminalP     <?> "Leg departure terminal"
  aircraft <- aircraftTypeP <?> "Leg aircraft type"
  void $ P.take (20+5+10+9)
  transit <- transitFlowP   <?> "leg MCT status"
  void $ P.take 6
  iviH <- decimalP 1 <|> (P.char ' ' *> pure 0) <?> "Leg variation (high)"
  void $ P.take (3+3+3+9+1+1+1)
  traffic <- legRestrictionsP <?> "Leg traffic restriction"
  void $ P.take (11+20)
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
                     srv freq dterm aterm
                     aircraft transit traffic

-- | Parser for traffic restriction at segment data level.
dataRestrictionP :: Parser Restriction
dataRestrictionP = restrictionP <* P.take 154

-- | Parser for segment data element.
dataElementP :: Int -> Parser SegmentDataElement
dataElementP n = case n of
  170 -> MkDEI17x . mkRestrictPax       <$> dataRestrictionP
  171 -> MkDEI17x . mkRestrictCargoMail <$> dataRestrictionP
  172 -> MkDEI17x . mkRestrictCargo     <$> dataRestrictionP
  173 -> MkDEI17x . mkRestrictMail      <$> dataRestrictionP
  710 -> pure (MkDEI71x True  False) <* ignored
  711 -> pure (MkDEI71x False True)  <* ignored
  712 -> pure (MkDEI71x True  True)  <* ignored
  _   -> ignored
  where ignored = pure IgnoredElement <* P.take 155

-- | Parser for board and off points indicators.
segmentIndexP :: Parser SegmentIndex
segmentIndexP = MkSegmentIndex <$> packWith 2 alphaPack
            <?> "Board and off points indicator"

-- | Parser for segment records.
segmentP :: Parser SegmentData
segmentP = do
  void (P.char '4')         <?> "Segment record type"
  suffix <- P.anyChar       <?> "Segment operational suffix"
  airline <- airlineP       <?> "Segment airline code"
  fnum <- fnumP             <?> "Segment flight number"
  iviL <- decimalP 2        <?> "Segment variation (low)"
  _lsn <- decimalP 2        <?> "Segment leg sequence number"
  void $ P.anyChar
  void $ P.take 13
  iviH <- decimalP 1 <|> (P.char ' ' *> pure 0) <?> "Segment variation (high)"
  idx <- segmentIndexP      <?> "Segment points indicator"
  dei <- decimalP 3         <?> "Segment data element indicator"
  bpoint <- portP           <?> "Segment board point"
  opoint <- portP           <?> "Segment off point"
  element <- dataElementP dei <?> "Segment data element"
  void $ P.take 6
  void $ some P.endOfLine
  let variation = iviL + 100 * iviH
      flight = Flight airline fnum suffix variation
  return $ SegmentData flight idx bpoint opoint element

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

-- | Extract segments from a flight
flightSegments :: FlightGroup -> [(OnD, SegmentPeriod)]
flightSegments = join . combine
  where combine []         = []
        combine xs@(x:xs') = [ mkAssoc y | y <- xs ] : combine xs'
          where legX = lgLeg x
                mkAssoc y = ((lpBoard legX, lpOff legY), map mkLeg legs)
                  where legY = lgLeg y
                        legs = takeWhile (on (>=) (lpSequence . lgLeg) $ y) xs
                        mkLeg l = MkSegmentLeg legL . map dElement
                                $ filter ((==idx) . dIndex) segL
                          where segL = lgSegments l
                                legL = lgLeg l
                                idx = segmentIndex (lpSequence legL)
                                                   (lpSequence legY)

-- | A leg is regular if servicing pax on a weekly basis.
isRegularLeg :: SegmentLeg -> Bool
isRegularLeg sl = (lpService lp) == ServicePax
               && (lpFrequency lp) == 1
  where lp = slLeg sl

-- | A segment is regular if all legs are regular and no traffic
--   restriction deny local traffic.
isRegularSegment :: SegmentPeriod -> Bool
isRegularSegment s = all isRegularLeg s
                  && (isLocalAllowed $ spRestrictService s)

-- | Run the SSIM parser on the given file.
readSsimFile :: String -> IO Ssim
readSsimFile s = do
  result <- LP.parse ssimP <$> LB.readFile s
  case result of
    LP.Fail left ctx msg -> fail . unlines $ msg:(show $ LB.length left):ctx
    LP.Done _ ssim      -> return ssim

-- | Extract segments from a SSIM structure.
ssimRegularSegments :: Ssim -> [(OnD, SegmentPeriod)]
ssimRegularSegments ssim = do
  car <- ssimCarriers ssim
  flt <- cgFlights car
  seg <- flightSegments flt
  guard $ isRegularSegment $ snd seg
  return seg
