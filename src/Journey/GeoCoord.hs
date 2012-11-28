module Journey.GeoCoord (
      loadReferences
    , mkAirportRefs
    , mkCityRefs
    , adjacency
    , porToCountry
    , assocToCities
    ) where

import Control.Monad (join, guard, foldM)
import Data.Maybe (fromJust, mapMaybe, isJust)
import Control.Arrow ((***))
import Data.List (groupBy, minimumBy)
import Data.Function (on)
import Data.Functor ((<$>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import qualified Journey.EnumMap as M
import Journey.Route
import Journey.Types
import Journey.Parsers

{-
  Geographic coordinates space
-}

newtype GeoCoord = GeoCoord (Double, Double) deriving (Show)

-- | The orthodromic distance between two geographic coordinates.
orthodromicDistance :: GeoCoord -> GeoCoord -> Distance
orthodromicDistance (GeoCoord (latA, lonA)) (GeoCoord (latB, lonB)) = dist
  where dist = 2.0 * radius * (asin . sqrt $ sin2_dLat + cos2_lat * sin2_dLon)
        sin2_dLat = (^(2::Int)) . sin $ (latB - latA) / 2.0
        sin2_dLon = (^(2::Int)) . sin $ (lonB - lonA) / 2.0
        cos2_lat = cos latA * cos latB
        radius = 6367.0

instance MetricSpace GeoCoord where
  distance = orthodromicDistance

-- | Make geographic coordinates from a pair of latitude and longitude.
fromDegree :: (Double, Double) -> GeoCoord
fromDegree (lat, lon) = GeoCoord (radian lat, radian lon)
  where radian d = d * pi / 180.0

-- | Category of point of reference.
data PorCategory = MkPorCategory { isAirportRef :: Bool
                                 , isCityRef :: Bool
                                 } deriving (Show)

-- | Geographic reference data.
data Reference = Reference { rPOR :: Port
                           , rCoord :: GeoCoord
                           , rCity  :: Port
                           , rCountry :: Country
                           , rCategory :: PorCategory
                           } deriving (Show)

-- | Port references.
type PortReferences = PortMap Reference

-- | Load airports information from a file.
loadReferences :: String -> IO [Reference]
loadReferences f = return . mapMaybe parse . drop 1 . T.lines =<< T.readFile f
  where parse row = do
          let col = V.fromList $ T.split (=='^') row
              por = fromJust . toPort . T.encodeUtf8 $ col V.! 0
              lat = read . T.unpack $ col V.! 7
              lon = read . T.unpack $ col V.! 8
              country = fromJust . toCountry . T.encodeUtf8 $ col V.! 11
              city = fromJust . toPort . T.encodeUtf8 $ col V.! 31
              category = mkCat $ col V.! 34
          return $ Reference por (fromDegree (lat, lon)) city country category

        mkCat t = MkPorCategory (isJust $ T.find (=='A') t)
                                (isJust $ T.find (=='C') t)

-- | Make a lookup structure for airports.
mkAirportRefs :: [Reference] -> PortReferences
mkAirportRefs = M.fromList
              . map (\r -> (rPOR r, r))
              . filter (isAirportRef . rCategory)

-- | Make a lookup structure for cities.
mkCityRefs :: [Reference] -> PortReferences
mkCityRefs = M.fromList
           . map (\r -> (rCity r, r))
           . filter (isCityRef . rCategory)

-- | Country from a point of reference.
porToCountry :: PortReferences -> Port -> Country
porToCountry pdb por = rCountry $ M.find por pdb

-- | Filter for city OnD.
citiesOnDFilter :: PortReferences -> OnD -> Maybe OnD
citiesOnDFilter pdb (a,b) = do
  a' <- rCity <$> M.lookup a pdb
  b' <- rCity <$> M.lookup b pdb
  return (a', b')

-- | City associations from port associations.
assocToCities :: PortReferences -> [(OnD, a)] -> [(OnD, a)]
assocToCities pdb = mapMaybe assoc
  where assoc (ond, x) = flip (,) x <$> citiesOnDFilter pdb ond

-- | Lookup points of reference in a path.
pathToRefs :: PortReferences -> Path -> Maybe [Reference]
pathToRefs pdb = mapM $ flip M.lookup pdb

-- | Geographic path length.
pathLength :: [Reference] -> Distance
pathLength = sum . map dist . steps
  where steps p = zip p (tail p)
        dist (a,b) = distance (rCoord a) (rCoord b)

-- | Ports adjacency in geographic coordinates.
adjacency :: PortReferences -> [(OnD, [Path])] -> PortAdjacencies GeoCoord
adjacency pdb = M.group . mapMaybe edge . filter valid
  where valid ((a,b), _) = a /= b
        edge ((a,b), paths) = do
          refs <- case mapMaybe (pathToRefs pdb) paths of
                    [] -> Nothing
                    rs -> Just rs
          let (best, dist) = minimumBy (compare `on` snd)
                           $ map (\rs -> (rs, pathLength rs)) refs
              co = rCoord $ head best
              cd = rCoord $ last best
          return (b, Edge a co cd dist)
