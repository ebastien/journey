{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.Foldable (foldMap)
import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy.Builder (toLazyText)
import System.Environment (getArgs)

import Journey.Ssim (readSsimFile, ssimRegularSegments)
import Journey.MCT.OAGParser (readMCTFile)
import Journey.MCT.Attributes (attributes)
import Journey.MCT.Tree (fromList, pruneLookup)
import Journey.Route (coverages)
import Journey.GeoCoord (loadReferences, assocToCities, adjacency, portToCountry)
import Journey.Builder (buildAll, buildPathPeriod)
import Journey.OnDSegments (fromSegments, toOnDPaths, fromOnD)

main :: IO ()
main = do
  [refsFile, mctFile, ssimFile] <- getArgs
  refs <- loadReferences refsFile
  mctdb <- fromList attributes <$> readMCTFile mctFile
  segdb <- fromSegments . assocToCities refs . ssimRegularSegments
       <$> readSsimFile ssimFile

  let covs = take 3 . coverages . adjacency refs $ toOnDPaths segdb
      segs = fromOnD segdb
      geos = portToCountry refs
      regn = pruneLookup mctdb

  T.putStr . toLazyText . buildAll covs $ buildPathPeriod segs geos regn
