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
import Journey.MCT.OAGParser (readMCTFile)
import Journey.MCT.Attributes (attributes)
import Journey.MCT.Tree (fromList)
import Journey.Parsers (toDate)
import Journey.Route (coverages)
import Journey.GeoCoord (loadReferences, assocToCities, adjacency)
import Journey.Connection (fromSegments, toOnDs)
import Journey.Builder (buildAll, buildPathDate, buildPathPeriod)

main :: IO ()
main = do
  [refsFile, mctFile, ssimFile, beginDate, endDate] <- getArgs
  refs <- loadReferences refsFile
  mctdb <- fromList attributes <$> readMCTFile mctFile
  segdb <- fromSegments . assocToCities refs . ssimSegments <$> readSsimFile ssimFile

  let covs = take 3 . coverages . adjacency refs $ toOnDs segdb
      dateL = fromJust . toDate $ pack beginDate
      dateH = fromJust . toDate $ pack endDate

--  let builder d = buildAll covs $ buildPathDate segdb d
--  T.putStr . toLazyText $ foldMap builder [dateL..dateH]

  T.putStr . toLazyText . buildAll covs $ buildPathPeriod segdb
