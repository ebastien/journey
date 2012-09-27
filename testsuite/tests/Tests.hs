module Main where

import Data.Functor ((<$>))
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.ByteString.Char8 (pack)

import Criterion.Main

import Journey.MCT.Attributes (attributes)
import Journey.MCT.Tree (fromList, pruneLookup)
import Journey.MCT.OAGParser

main :: IO ()
main = do
  [mctFile, rule] <- getArgs
  mctdb <- fromList attributes <$> readMCTFile mctFile
  let rule' = fromJust . toRule . pack $ rule ++ "\n"
  defaultMain [ bench "lookup" $ whnf (pruneLookup $! mctdb) rule' ]
