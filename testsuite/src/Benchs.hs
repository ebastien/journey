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
import Journey.GeoCoord (loadReferences, assocToCities, adjacency, portToCountry)
import Journey.Builder (buildSplit, buildPathPeriod)
import Journey.OnDSegments (fromSegments, toOnDs, fromOnD)
import Journey.Parsers (toPort)

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
  mctdb <- fromList attributes <$> readMCTFile mctFile
  segdb <- fromSegments . assocToCities refs . ssimRegularSegments
       <$> readSsimFile ssimFile

  let covs = take 3 . coverages . adjacency refs $ toOnDs segdb
      segs = fromOnD segdb
      geos = portToCountry refs
      regn = pruneLookup mctdb
      bldr = buildPathPeriod segs geos regn

  thds <- getNumCapabilities

  parallelBuild thds $ buildSplit covs bldr 10000
