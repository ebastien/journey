{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Journey.MCT.Attributes (attributes) where

import Data.Maybe (isJust, fromJust, maybeToList)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntervalMap as PM

import qualified Journey.EnumMap as EM
import Journey.Types (Port, POnD(..), AirlineCode, City, Region, State, Country, Transit)
import Journey.MCT.DecisionTree
import Journey.MCT.Tree
import Journey.MCT.Rule

-- | A container for Int attributes.
type IntStore = IM.IntMap MCTTree

emptyInt :: IntStore
emptyInt = IM.empty

storeInt :: IntStore -> Int -> (MCTTree -> MCTTree) -> IntStore
storeInt s k c = IM.insertWith f k (c Empty) s
  where f _ = c

fetchInt :: IntStore -> Int -> [MCTTree]
fetchInt s k = maybeToList $ IM.lookup k s

-- | A container for Enum attributes.
type EnumStore e = EM.EnumMap e MCTTree

emptyEnum :: EnumStore e
emptyEnum = EM.empty

storeEnum :: Enum e => EnumStore e -> e -> (MCTTree -> MCTTree) -> EnumStore e
storeEnum s k c = EM.insertWith f k (c Empty) s
  where f _ = c

fetchEnum :: Enum e => EnumStore e -> e -> [MCTTree]
fetchEnum s k = maybeToList $ EM.lookup k s

-- | A container for range attributes.
type RangeStore e = PM.IntervalMap e MCTTree

class Ord e => IsRange e a where
  fromRange :: a -> PM.Interval e

instance (Ord a, Bounded a) => IsRange a (a, Maybe a) where
  fromRange (a, b) = PM.ClosedInterval a $ case b of
                                             Nothing -> a
                                             Just c  -> c

instance (Ord a, Bounded a) => IsRange a (Maybe a, Maybe a) where
  fromRange (a, b) = PM.ClosedInterval x y
    where x = case a of
                Nothing -> minBound
                Just d  -> d
          y = case b of
                Nothing -> maxBound
                Just d  -> d

emptyRange :: RangeStore e
emptyRange = PM.empty

storeRange:: IsRange e a => RangeStore e -> a -> (MCTTree -> MCTTree) -> RangeStore e
storeRange s r c = PM.insertWith f (fromRange r) (c Empty) s
  where f _ = c

fetchRange :: IsRange e a => RangeStore e -> a -> [MCTTree]
fetchRange s r = map snd $ PM.intersecting s (fromRange r)

-- | A container for Int range attributes.
type IntRgStore = RangeStore Int
type IntRg = (Int, Maybe Int)

-- | A container for Period attributes.
type PeriodStore = RangeStore Day
type Period = (Maybe Day, Maybe Day)

instance Bounded Day where
  minBound = fromGregorian 1900 1 1
  maxBound = fromGregorian 9999 1 1

options :: MinMCT -> Options
options = rOptions . getMinMCT

data ConnectingPorts

class IsAttribute a where
  type StoredAttr a :: *

  maybeAttribute :: Store_ MinMCT a -> MinMCT -> Maybe (StoredAttr a)

  justAttribute :: Store_ MinMCT a -> MinMCT -> StoredAttr a
  justAttribute s = fromJust . maybeAttribute s

  hasAttribute :: Store_ MinMCT a -> MinMCT -> Bool
  hasAttribute s = isJust . maybeAttribute s

instance IsAttribute ConnectingPorts where
  type StoredAttr ConnectingPorts = POnD
  maybeAttribute _ k = f (rArrPort o) (rDepPort o)
    where o = options k
          f a b = case a of
                    Nothing -> Nothing
                    Just x  -> Just $ case b of
                                        Nothing -> MkPOnD x x
                                        Just y  -> MkPOnD x y

instance IsAttribute ConnectingPorts => Attribute MinMCT ConnectingPorts where
  newtype Store_ MinMCT ConnectingPorts = MkSConnectingPorts { getSConnectingPorts :: EnumStore POnD }
                                          deriving (Show)
  empty_ = MkSConnectingPorts emptyEnum
  store_ s k c = MkSConnectingPorts
               $ storeEnum (getSConnectingPorts s) (justAttribute s k) c
  fetch_ s k = fetchEnum (getSConnectingPorts s) (justAttribute s k)
  exist_ = hasAttribute
 
data ValidityPeriod

instance Attribute MinMCT ValidityPeriod where
  data Store_ MinMCT ValidityPeriod = MkSValidityPeriod PeriodStore
                                      deriving (Show)
  empty_ = MkSValidityPeriod emptyRange
  store_ (MkSValidityPeriod s) k c = MkSValidityPeriod
                                   $ storeRange s ( rValidityBegin $ options k
                                                  , rValidityEnd $ options k ) c
  fetch_ (MkSValidityPeriod s) k = fetchRange s ( rValidityBegin $ options k
                                                , rValidityEnd $ options k)
  exist_ _ k = isJust (rValidityBegin o) || isJust (rValidityEnd o)
    where o = options k

data ArrivalFlightRange

instance Attribute MinMCT ArrivalFlightRange where
  data Store_ MinMCT ArrivalFlightRange = MkSArrivalFlightRange IntRgStore
                                          deriving (Show)
  empty_ = MkSArrivalFlightRange emptyRange
  store_ (MkSArrivalFlightRange s) k c = MkSArrivalFlightRange
                                       $ storeRange s ( fromJust . rArrFlightBegin $ options k
                                                      , rArrFlightEnd $ options k ) c
  fetch_ (MkSArrivalFlightRange s) k = fetchRange s ( fromJust . rArrFlightBegin $ options k
                                                    , rArrFlightEnd $ options k)
  exist_ _ k = isJust . rArrFlightBegin $ options k

data ArrivalTerminal
data DepartureTerminal
data TransitStatus
data ArrivalCarrier
data DepartureCarrier
data PreviousRegion
data NextRegion
data PreviousCountry
data NextCountry
data PreviousState
data NextState
data PreviousCity
data NextCity
data PreviousAirport
data NextAirport
data DepartureFlightRange
data ArrivalAircraftBody
data DepartureAircraftBody
data ArrivalAircraftType
data DepartureAircraftType
             
-- | Structure of MCT attributes.
attributes :: [MCTStorable]
attributes = [
{-
               MkStorable (empty :: Store MinMCT ConnectingPorts)
             , MkStorable (empty :: Store MinMCT ArrivalTerminal)
             , MkStorable (empty :: Store MinMCT DepartureTerminal)
             , MkStorable (empty :: Store MinMCT TransitStatus)
             , MkStorable (empty :: Store MinMCT ArrivalCarrier)
             , MkStorable (empty :: Store MinMCT DepartureCarrier)
             , MkStorable (empty :: Store MinMCT PreviousRegion)
             , MkStorable (empty :: Store MinMCT NextRegion)
             , MkStorable (empty :: Store MinMCT PreviousCountry)
             , MkStorable (empty :: Store MinMCT NextCountry)
             , MkStorable (empty :: Store MinMCT PreviousState)
             , MkStorable (empty :: Store MinMCT NextState)
             , MkStorable (empty :: Store MinMCT PreviousCity)
             , MkStorable (empty :: Store MinMCT NextCity)
             , MkStorable (empty :: Store MinMCT PreviousAirport)
             , MkStorable (empty :: Store MinMCT NextAirport)
             , MkStorable (empty :: Store MinMCT ArrivalFlightRange)
             , MkStorable (empty :: Store MinMCT DepartureFlightRange)
             , MkStorable (empty :: Store MinMCT ArrivalAircraftBody)
             , MkStorable (empty :: Store MinMCT DepartureAircraftBody)
             , MkStorable (empty :: Store MinMCT ArrivalAircraftType)
             , MkStorable (empty :: Store MinMCT DepartureAircraftType)
             , MkStorable (empty :: Store MinMCT ValidityPeriod)
-}
             ]
