{-# LANGUAGE OverloadedStrings #-}

module Journey.MCT.OAGParser () where

import Data.Functor ((<$>))
import Control.Applicative (pure, some, (<*>), (<*), (*>), (<|>))
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP
import qualified Data.ByteString.Lazy as LB

import Journey.Types
import Journey.Parsers
import Journey.MCT.Rule

data Database = Database { rules :: [Rule] } deriving Show

ruleP :: Parser Rule
ruleP = do
  arrPort <- (P.string "***" *> pure Nothing) <|> (Just <$> portP)
  void $ P.string "   "
  mct <- decimalP 3
  transit <- transitP
  ports <- case arrPort of
             Nothing -> (P.string "   " *> pure (SamePort Nothing))
                    <|> (P.string "***" *> pure (OtherPort Nothing))
             Just a  -> (P.string "   " *> pure (SamePort arrPort))
                    <|> (otherPort arrPort <$> portP)
  let rank = MkRank 0
      options = MkOptions ports
                          arrTerminal depTerminal
                          transit
                          arrCarrier depCarrier
                          prevRegion nextRegion
                          prevCountry nextCountry
                          prevState nextState
                          prevCity nextCity
                          prevPort nextPort
                          arrFlightBegin arrFlightEnd
                          depFlightBegin depFlightEnd
                          arrAircraftBody depAircraftBody
                          arrAircraftType depAircraftType
                          validityBegin validityEnd
  return $ MkRule rank mct options

databaseP :: Parser Database
databaseP = Database <$> some ruleP <?> "OAG MCT rules"

-- | Run the MCT parser on the given file.
loadDatabase :: String -> IO Database
loadDatabase s = do
  result <- LP.parse databaseP <$> LB.readFile s
  case result of
    LP.Fail left ctx msg -> fail . unlines $ msg:(show $ LB.length left):ctx
    LP.Done _ db         -> return db

