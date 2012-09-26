{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor ((<$>))
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.ByteString.Char8 (pack)

import Journey.MCT.Attributes (attributes)
import Journey.MCT.Tree (fromList, pruneLookup)
import Journey.MCT.OAGParser

main :: IO ()
main = do
  [mctFile, rule] <- getArgs
  mctdb <- fromList attributes <$> readMCTFile mctFile
  putStrLn . show $ pruneLookup mctdb (fromJust . toRule . pack $ rule ++ "\n")
