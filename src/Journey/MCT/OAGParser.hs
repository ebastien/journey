{-# LANGUAGE OverloadedStrings #-}

module Journey.MCT.OAGParser () where

import Data.Functor ((<$>))
import Control.Applicative (pure, some, (<*>), (<*), (*>), (<|>))
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P

import Journey.Types
import Journey.Parsers
import Journey.MCT.Rule

data Database = Database { rules :: [Rule] } deriving Show

ruleP :: Parser Rule
ruleP = do
  _arrPort <- (P.string "***" *> pure Nothing) <|> (Just <$> portP)
  let rank = undefined
      mct = undefined
      options = undefined
  return $ MkRule rank mct options

databaseP :: Parser Database
databaseP = Database <$> some ruleP <?> "OAG MCT rules"
