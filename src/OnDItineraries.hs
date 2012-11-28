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
import Journey.GeoCoord (loadReferences, assocToCities, adjacency, porToCountry,
                         mkAirportRefs, mkCityRefs)
import Journey.Builder (buildForSome, buildForOnD, buildForPath)
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
  let airports = mkAirportRefs refs
      cities = mkCityRefs refs
  
  mctdb <- fromList attributes <$> readMCTFile mctFile
  segdb <- fromSegments . assocToCities airports . ssimRegularSegments
       <$> readSsimFile ssimFile
  onds <- readOnDs ondsFile

  let covs = take 3 . coverages . adjacency airports $ toOnDPaths segdb
      segs = fromOnD segdb
      geos = porToCountry airports
      regn = pruneLookup mctdb
      cntr = connectionsPeriod segs geos regn

  LT.putStr . toLazyText $ buildForSome covs onds (\o ->
                             buildForOnD covs o (\p ->
                               buildForPath o p (cntr p) geos))
