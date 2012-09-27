module Main where

import Data.Functor ((<$>))
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.ByteString.Char8 (pack)

import Criterion.Main

import Journey.MCT.Attributes (attributes)
import Journey.MCT.Tree (fromList, pruneLookup)
import Journey.MCT.OAGParser
import Journey.MCT.Rule

main :: IO ()
main = do
  [mctFile, rule] <- getArgs
  mctdb <- fromList attributes <$> readMCTFile mctFile
  let rule' = fromJust . toRule . pack $ rule ++ "\n"
  putStrLn . show $ pruneLookup mctdb rule'
  -- defaultMain [ bench "lookup" $ whnf (fromJust . pruneLookup mctdb) rule' ]
