{-# LANGUAGE OverloadedStrings #-}

module Journey.MCT.OAGParser () where

import Data.Functor ((<$>))
import Control.Monad (void)
import Data.Char (ord, chr)
import Control.Applicative (pure, some, (<*>), (<*), (*>), (<|>))
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.ByteString.Char8 as B8
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP
import qualified Data.ByteString.Lazy as LB

import Journey.Types
import Journey.Parsers
import Journey.MCT.Rule

data Database = Database { rules :: [Rule] } deriving Show

option :: Parser a -> Parser (Maybe a)
option p = (Just <$> p) <|> (pure Nothing)

countryP :: Parser Country
countryP = MkCountry <$> packWith 2 alphaPack

regionP :: Parser Region
regionP = MkRegion <$> packWith 3 alphaPack

stateP :: Parser State
stateP = MkState <$> packWith 2 alphaPack

transitAreaP :: Parser TransitArea
transitAreaP = P.char 'D' *> pure Domestic
           <|> P.char 'I' *> pure International

transitFlowP :: Parser TransitFlow
transitFlowP = MkTransitFlow <$> transitAreaP <*> transitAreaP

peekEndOfLine :: Parser ()
peekEndOfLine = do
  c <- P.peekChar
  case c of
    Just '\n' -> pure ()
    _         -> fail "No end of line"

terminalP :: Parser Terminal
terminalP = MkTerminal <$> packBoundedWith 1 2 peekEndOfLine alphaNumPack

aircraftBodyP :: Parser AircraftBody
aircraftBodyP = P.string "  N" *> pure Narrow
            <|> P.string "  W" *> pure Wide

aircraftTypeP :: Parser AircraftType
aircraftTypeP = MkAircraftType <$> packWith 3 alphaNumPack

-- | Parser for MCT carrier codes.
carrierP :: Parser AirlineCode
carrierP = MkAirlineCode <$> packBoundedWith 2 3 peekEndOfLine step <?> "Airline code"
  where step n = (* 37^n) <$> code n
        code n | n < 2     = letter <|> digit
               | otherwise = letter <|> digit <|> space
        letter = (+11) . subtract (ord 'A') . ord <$> P.satisfy isUpperLetter
        digit  =  (+1) . subtract (ord '0') . ord <$> P.digit
        space  = pure 0 <* P.char ' '
        isUpperLetter c = c >= 'A' && c <= 'Z'

type AircraftClass = Either AircraftBody AircraftType

aircraftClassP :: Parser AircraftClass
aircraftClassP = (Left <$> aircraftBodyP) <|> (Right <$> aircraftTypeP)

fromClass :: Maybe AircraftClass -> (Maybe AircraftBody, Maybe AircraftType)
fromClass c = case c of
                Nothing        -> (Nothing, Nothing)
                Just (Left a)  -> (Just a , Nothing)
                Just (Right a) -> (Nothing, Just a )

spacePadding :: Int -> Parser ()
spacePadding n = void (P.string $ B8.replicate n ' ')
             <|> peekEndOfLine <?> "Space padding"

flightsP :: Parser (Maybe (Int, Int))
flightsP = do
  begin <- option fnumP <?> "Low bound flight number"
  case begin of
    Nothing -> spacePadding 4 *>  pure Nothing <?> "High bound flight number (none)"
    Just a  -> spacePadding 4 *> (pure $ Just (a,a))
           <|> Just . (,) a <$> fnumP

ruleP :: Parser Rule
ruleP = do
  arrPort <- (P.string "***" *> pure Nothing)
         <|> (Just <$> portP) <?> "Rule arrival airport"
  void (P.string "   ")       <?> "Rule sequence number"
  mct <- MkMCT <$> decimalP 3 <?> "Rule MCT"
  transit <- transitFlowP     <?> "Rule transit"
  ports <- case arrPort of
             Nothing -> spacePadding 3 *> (pure $ SameTransitPort Nothing)
                    <|> P.string "***" *> (pure $ OtherTransitPort Nothing)
             Just a  -> spacePadding 3 *> (pure $ SameTransitPort arrPort)
                    <|> otherTransitPort a <$> portP
           <?> "Rule departure airport"
  arrCarrier <- option carrierP         <?> "Rule arrival carrier"
  depCarrier <- option carrierP         <?> "Rule departure carrier"
  arrAircraft <- option aircraftClassP  <?> "Rule arrival aircraft"
  depAircraft <- option aircraftClassP  <?> "Rule departure aircraft"
  arrTerminal <- option terminalP       <?> "Rule arrival terminal"
  depTerminal <- option terminalP       <?> "Rule departure terminal"
  prevCountry <- option countryP        <?> "Rule previous country"
  prevCity <- option portP              <?> "Rule previous city"
  prevPort <- option portP              <?> "Rule previous airport"
  nextCountry <- option countryP        <?> "Rule next country"
  nextCity <- option portP              <?> "Rule next city"
  nextPort <- option portP              <?> "Rule next airport"
  arrFlights <- flightsP                <?> "Rule arrival flight numbers"
  depFlights <- flightsP                <?> "Rule departure flight numbers"
  prevState <- option stateP            <?> "Rule previous state"
  nextState <- option stateP            <?> "Rule next state"
  prevRegion <- option regionP          <?> "Rule previous region"
  nextRegion <- option regionP          <?> "Rule next region"
  validityBegin <- option dateP         <?> "Rule effective date"
  validityEnd <- option dateP           <?> "Rule discontinue date"
  let rank = MkRank 0
      (arrAircraftBody, arrAircraftType) = fromClass arrAircraft
      (depAircraftBody, depAircraftType) = fromClass depAircraft
      options = MkOptions ports
                          arrTerminal depTerminal
                          transit
                          arrCarrier depCarrier
                          prevRegion nextRegion
                          prevCountry nextCountry
                          prevState nextState
                          prevCity nextCity
                          prevPort nextPort
                          arrFlights depFlights
                          arrAircraftBody depAircraftBody
                          arrAircraftType depAircraftType
                          validityBegin validityEnd
  return $ MkRule rank mct options

databaseP :: Parser Database
databaseP = Database <$> some (ruleP <* P.endOfLine) <?> "OAG MCT rules"

-- | Run the MCT parser on the given file.
loadDatabase :: String -> IO Database
loadDatabase s = do
  result <- LP.parse databaseP <$> LB.readFile s
  case result of
    LP.Fail left ctx msg -> fail . unlines $ msg:(show $ LB.length left):ctx
    LP.Done _ db         -> return db

