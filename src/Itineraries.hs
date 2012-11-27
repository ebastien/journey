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
import Journey.Builder (buildForAll, buildForOnD, buildForPath)
import Journey.OnDSegments (fromSegments, toOnDPaths, fromOnD)
import Journey.Connection (connectionsPeriod)

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
      cntr = connectionsPeriod segs geos regn

  T.putStr . toLazyText $ buildForAll covs (\o ->
                            buildForOnD covs o (\p ->
                              buildForPath o p (cntr p)))
