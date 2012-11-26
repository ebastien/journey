{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.Foldable (foldMap)
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Text.Lazy.Builder (toLazyText)
import System.Environment (getArgs)

import Journey.Ssim (readSsimFile, ssimRegularSegments)
import Journey.MCT.OAGParser (readMCTFile)
import Journey.MCT.Attributes (attributes)
import Journey.MCT.Tree (fromList, pruneLookup)
import Journey.Route (coverages)
import Journey.GeoCoord (loadReferences, assocToCities, adjacency, portToCountry)
import Journey.Builder (buildForOnD, buildAllPaths)
import Journey.OnDSegments (fromSegments, toOnDPaths, fromOnD)
import Journey.Types (OnD)
import Journey.Parsers (toPort)
import Journey.Connection (connectionsPeriod)

readOnDs :: String -> IO [OnD]
readOnDs f = return . map parse . T.lines =<< T.readFile f
  where parse row = let (a:b:[]) = T.split (==',') row in (port a, port b)
        port = fromJust . toPort . T.encodeUtf8

main :: IO ()
main = do
  [refsFile, mctFile, ssimFile, ondsFile] <- getArgs
  refs <- loadReferences refsFile
  mctdb <- fromList attributes <$> readMCTFile mctFile
  segdb <- fromSegments . assocToCities refs . ssimRegularSegments
       <$> readSsimFile ssimFile
  onds <- readOnDs ondsFile

  let covs = take 3 . coverages . adjacency refs $ toOnDPaths segdb
      segs = fromOnD segdb
      geos = portToCountry refs
      regn = pruneLookup mctdb
      cntr = connectionsPeriod segs geos regn
      bldr = buildForOnD covs cntr

  -- LT.putStr . toLazyText $ buildAllPaths covs
  LT.putStr . toLazyText $ foldMap bldr onds
