module Journey.GeoCoord (
      loadReferences
    , adjacency
    , portToCountry
    , assocToCities
    ) where

import Control.Monad (join, guard, foldM)
import Data.Maybe (fromJust, mapMaybe, isJust)
import Control.Arrow ((***))
import Data.List (groupBy)
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

-- | Geographic reference data.
data Reference = Reference { rCoord :: GeoCoord
                           , rCity  :: Port
                           , rCountry :: Country
                           } deriving (Show)

-- | Port references.
type PortReferences = PortMap Reference

-- | Load airports information from a file.
loadReferences :: String -> IO PortReferences
loadReferences f = return . M.fromList . mapMaybe parse . drop 1 . T.lines =<< T.readFile f
  where parse row = do
          let col = V.fromList $ T.split (=='^') row
              port = fromJust . toPort . T.encodeUtf8 $ col V.! 0
              lat = read . T.unpack $ col V.! 7
              lon = read . T.unpack $ col V.! 8
              country = fromJust . toCountry . T.encodeUtf8 $ col V.! 11
              city = fromJust . toPort . T.encodeUtf8 $ col V.! 31
              category = col V.! 34
          guard . isJust $ T.find (=='A') category
          return (port, Reference (fromDegree (lat, lon)) city country)

-- | Country from a port.
portToCountry :: PortReferences -> Port -> Country
portToCountry refs port = rCountry $ M.find port refs

-- | Filter for city OnD.
citiesOnDFilter :: PortReferences -> OnD -> Maybe OnD
citiesOnDFilter refs (a,b) = do
  a' <- M.lookup a refs
  b' <- M.lookup b refs
  return (rCity a', rCity b')

-- | City associations from port associations.
assocToCities :: PortReferences -> [(OnD, a)] -> [(OnD, a)]
assocToCities refs = mapMaybe assoc
  where assoc (ond, x) = flip (,) x <$> citiesOnDFilter refs ond

-- | Geographic path length.
pathLength :: PortReferences -> Path -> Maybe Distance
pathLength refs p = foldM f 0 $ zip p (tail p)
  where f l (o,d) = do
          co <- rCoord <$> M.lookup o refs
          cd <- rCoord <$> M.lookup d refs
          return $ l + distance co cd

-- | Ports adjacency in geographic coordinates.
adjacency :: PortReferences -> [(OnD, [Path])] -> PortAdjacencies GeoCoord
adjacency refs = M.group . mapMaybe edge . filter valid
  where valid ((o,d), _) = o /= d
        edge ((o,d), paths) = do
          dist <- case mapMaybe (pathLength refs) paths of
                    [] -> Nothing
                    ps -> Just $ minimum ps
          co <- rCoord <$> M.lookup o refs -- This is wrong as we are mixing
          cd <- rCoord <$> M.lookup d refs -- city and airport codes
          return (d, Edge o co cd dist)
