{-# LANGUAGE OverloadedStrings #-}

module Journey.MCT.OAGParser (
    readMCTFile
  , toRule
  ) where

import Data.Functor ((<$>))
import Data.Maybe (catMaybes, isJust)
import Control.Monad (void)
import Data.Char (ord, chr)
import Control.Applicative (pure, some, (<*>), (<*), (*>), (<|>))
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.ByteString.Char8 as B8
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP
import qualified Data.Attoparsec.ByteString as PW
import qualified Data.ByteString.Lazy as LB

import Journey.Types
import Journey.Parsers
import Journey.MCT.Rule

type AircraftClass = Either AircraftBody AircraftType

peekEndOfLine :: Parser ()
peekEndOfLine = P.endOfInput <|> do
  c <- P.peekChar
  case c of
    Just '\n' -> pure ()
    _         -> fail "No end of line"

spaceP :: Int -> Parser ()
spaceP n = void (P.string $ B8.replicate n ' ')
       <|> peekEndOfLine <?> "Space padding"

defaultP :: Int -> a -> Parser a
defaultP n a = spaceP n *> pure a

option :: Int -> Parser a -> Parser (Option a)
option n p = Known <$> p <|> defaultP n Unknown

alphaNumPackBounded :: Int -> Int -> Parser Int
alphaNumPackBounded l h = packBoundedWith l h peekEndOfLine (alphaNumPack l)

mctTerminalP :: Parser Terminal
mctTerminalP = MkTerminal <$> alphaNumPackBounded 1 2
          <?> "Terminal"

aircraftBodyP :: Parser AircraftBody
aircraftBodyP = P.char 'N' *> defaultP 2 Narrow
            <|> P.char 'W' *> defaultP 2 Wide
            <?> "Aircraft body"

-- | Try to convert a ByteString to a AircraftBody.
toAircraftBody :: B8.ByteString -> Maybe AircraftBody
toAircraftBody = maybeParse aircraftBodyP

-- | Parser for MCT carrier codes.
carrierP :: Parser AirlineCode
carrierP = MkAirlineCode <$> alphaNumPackBounded 2 3 <?> "Carrier"

aircraftClassP :: Parser AircraftClass
aircraftClassP = Left <$> aircraftBodyP <|> Right <$> aircraftTypeP

-- TODO: Move to attributes
fromClass :: Option AircraftClass -> (Option AircraftBody, Option AircraftType)
fromClass c = case c of
                Unknown         -> (Unknown, Unknown)
                Known (Left a)  -> (Known a, Unknown)
                Known (Right a) -> (Unknown, Known a)

flightsP :: Parser (Option FlightRange)
flightsP = do
  begin <- option 4 fnumP <?> "Low bound flight number"
  case begin of
    Unknown -> defaultP 4 Unknown      <?> "High bound flight number (none)"
    Known a -> defaultP 4 (Known $ MkFlightRange a a)
           <|> Known . MkFlightRange a <$> fnumP

headerP :: Parser ()
headerP = P.string "UHL"
       *> P.satisfy (\c -> c == '1' || c == '2')
       *> PW.skipWhile (not . P.isEndOfLine)
       *> P.endOfLine *> pure ()

countOption :: Option a -> Int
countOption a = if isKnown a then 1; else 0

ruleP :: Parser Rule
ruleP = do
  arrPort <- P.string "***" *> pure Unknown
         <|> Known <$> portP               <?> "Rule arrival airport"
  void (P.string "   ")                   <?> "Rule sequence number"
  mct <- MkMCT <$> decimalP 3             <?> "Rule MCT"
  transit <- transitFlowP                 <?> "Rule transit"
  (intra, ports) <- case arrPort of
             Unknown -> defaultP 3 (True, Unknown)
                    <|> P.string "***" *> pure (False, Unknown)
             Known a -> defaultP 3 (True, Known $ MkPOnD a a)
                    <|> (,) False . Known . MkPOnD a <$> portP
       <?> "Rule departure airport"
  arrCarrier <- option 3 carrierP         <?> "Rule arrival carrier"
  depCarrier <- option 3 carrierP         <?> "Rule departure carrier"
  arrAircraft <- option 3 aircraftClassP  <?> "Rule arrival aircraft"
  depAircraft <- option 3 aircraftClassP  <?> "Rule departure aircraft"
  arrTerminal <- option 2 mctTerminalP    <?> "Rule arrival terminal"
  depTerminal <- option 2 mctTerminalP    <?> "Rule departure terminal"
  prevCountry <- option 2 countryP        <?> "Rule previous country"
  prevCity <- option 3 portP              <?> "Rule previous city"
  prevPort <- option 3 portP              <?> "Rule previous airport"
  nextCountry <- option 2 countryP        <?> "Rule next country"
  nextCity <- option 3 portP              <?> "Rule next city"
  nextPort <- option 3 portP              <?> "Rule next airport"
  arrFlights <- flightsP                  <?> "Rule arrival flight numbers"
  depFlights <- flightsP                  <?> "Rule departure flight numbers"
  prevState <- option 2 stateP            <?> "Rule previous state"
  nextState <- option 2 stateP            <?> "Rule next state"
  prevRegion <- option 3 regionP          <?> "Rule previous region"
  nextRegion <- option 3 regionP          <?> "Rule next region"
  validityBegin <- option 7 dateP         <?> "Rule effective date"
  validityEnd <- option 7 dateP           <?> "Rule discontinue date"
  void P.endOfLine                        <?> "Rule end of line"
  let rank = MkRank $! countOption arrCarrier
                     + countOption depCarrier
                     + countOption arrAircraft
                     + countOption depAircraft
                     + countOption arrTerminal
                     + countOption depTerminal
                     + countOption prevCountry
                     + countOption prevCity
                     + countOption prevPort
                     + countOption nextCountry
                     + countOption nextCity
                     + countOption nextPort
                     + countOption arrFlights
                     + countOption depFlights
                     + countOption prevState
                     + countOption nextState
                     + countOption prevRegion
                     + countOption nextRegion
                     + countOption validityBegin
                     + countOption validityEnd
      (arrAircraftBody, arrAircraftType) = fromClass arrAircraft
      (depAircraftBody, depAircraftType) = fromClass depAircraft
      options = MkOptions intra ports
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
  return $! MkRule rank mct options

toRule :: B8.ByteString -> Maybe Rule
toRule = maybeParse ruleP

rowP :: Parser (Maybe Rule)
rowP = Just <$> ruleP <|> headerP *> pure Nothing

rulesP :: Parser [Rule]
rulesP = catMaybes <$> some rowP <?> "OAG MCT rules"

-- | Run the MCT parser on the given file.
readMCTFile :: String -> IO [Rule]
readMCTFile s = do
  result <- LP.parse rulesP <$> LB.readFile s
  case result of
    LP.Fail left ctx msg -> fail . unlines $ msg:(show $ LB.length left):ctx
    LP.Done _ db         -> return db

