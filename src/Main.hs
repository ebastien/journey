{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.Foldable (foldMap)
import Data.ByteString.Char8 (pack)
import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy.Builder (toLazyText)
import System.Environment (getArgs)

import Journey.Ssim (readSsimFile, ssimSegments)
import Journey.Parsers (toDate)
import Journey.Route (coverages)
import Journey.GeoCoord (loadReferences, assocToCities, adjacency)
import Journey.Connection (fromSegments, toOnDs)
import Journey.Builder (buildAll)

main :: IO ()
main = do
  [refsFile, ssimFile, beginDate, endDate] <- getArgs
  refs <- loadReferences refsFile
  segdb <- fromSegments . assocToCities refs . ssimSegments <$> readSsimFile ssimFile

  let covs = take 3 . coverages . adjacency refs $ toOnDs segdb
      dateL = fromJust . toDate $ pack beginDate
      dateH = fromJust . toDate $ pack endDate

  T.putStr . toLazyText $ foldMap (buildAll segdb covs) [dateL..dateH]
