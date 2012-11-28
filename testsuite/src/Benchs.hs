{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.Foldable (foldMap)
import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy.Builder (toLazyText, Builder)
import System.Environment (getArgs)
import Text.Printf (printf)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.ParallelIO.Local (withPool, parallel_)

import Journey.Ssim (readSsimFile, ssimRegularSegments)
import Journey.MCT.OAGParser (readMCTFile)
import Journey.MCT.Attributes (attributes)
import Journey.MCT.Tree (fromList, pruneLookup)
import Journey.Route (coverages)
import Journey.GeoCoord (loadReferences, assocToCities, adjacency, porToCountry,
                         mkAirportRefs, mkCityRefs)
import Journey.Builder (buildSplit, buildForOnD, buildForPath)
import Journey.OnDSegments (fromSegments, toOnDPaths, fromOnD)
import Journey.Parsers (toPort)
import Journey.Connection (connectionsPeriod)

buildInFile :: FilePath -> Builder -> IO ()
buildInFile f = T.writeFile f . toLazyText

parallelBuild :: Int -> [Builder] -> IO ()
parallelBuild n bs = withPool n $ \p -> parallel_ p chunks
  where chunks = uncurry buildInFile <$> zip files bs
        files = printf "part_%05d" <$> [0::Int ..]

main :: IO ()
main = do
  [refsFile, mctFile, ssimFile] <- getArgs

  refs <- loadReferences refsFile
  let airports = mkAirportRefs refs
      cities = mkCityRefs refs
  
  mctdb <- fromList attributes <$> readMCTFile mctFile
  segdb <- fromSegments . assocToCities airports . ssimRegularSegments
       <$> readSsimFile ssimFile

  let covs = take 3 . coverages . adjacency airports $ toOnDPaths segdb
      segs = fromOnD segdb
      geos = porToCountry airports
      regn = pruneLookup mctdb
      cntr = connectionsPeriod segs geos regn

  thds <- getNumCapabilities

  parallelBuild thds $ buildSplit 10000 covs (\o ->
                        buildForOnD covs o (\p ->
                          buildForPath o p (cntr p) geos))
