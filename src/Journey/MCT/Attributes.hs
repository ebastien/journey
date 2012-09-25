{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Journey.MCT.Attributes (attributes) where

import Data.Functor ((<$>))
import Data.Maybe (isJust, fromJust, maybeToList)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntervalMap as PM

import qualified Journey.EnumMap as EM
import Journey.Types
import Journey.MCT.DecisionTree
import Journey.MCT.Tree
import Journey.MCT.Rule

-- | A container for Int attributes.
instance IsStore MinMCT Int where
  newtype OptionStore MinMCT Int = MkMCTInt (IM.IntMap MCTTree)
  emptyStore = MkMCTInt $ IM.empty
  storeOption (MkMCTInt s) o c = let f _ = c
                                 in MkMCTInt
                                  $ IM.insertWith f o (c Empty) s
  fetchOption (MkMCTInt s) o = maybeToList $ IM.lookup o s

newtype OptionEnum e = MkOptionEnum { optionEnum :: e }

-- | A container for Enum attributes.
instance Enum e => IsStore MinMCT (OptionEnum e) where
  newtype OptionStore MinMCT (OptionEnum e) = MkMCTEnum (EM.EnumMap e MCTTree)
  emptyStore = MkMCTEnum $ EM.empty
  storeOption (MkMCTEnum s) o c = let f _ = c
                                  in MkMCTEnum
                                   $ EM.insertWith f (optionEnum o) (c Empty) s
  fetchOption (MkMCTEnum s) o = maybeToList $ EM.lookup (optionEnum o) s

-- | A container for range attributes.
instance Ord o => IsStore MinMCT (PM.Interval o) where
  newtype OptionStore MinMCT (PM.Interval o) = MkMCTRange (PM.IntervalMap o MCTTree)
  emptyStore = MkMCTRange $ PM.empty
  storeOption (MkMCTRange s) o c = let f _ = c
                                   in MkMCTRange
                                    $ PM.insertWith f o (c Empty) s
  fetchOption (MkMCTRange s) o = map snd $ PM.intersecting s o

options :: MinMCT -> Options
options = rOptions . getMinMCT

instance Bounded Day where
  minBound = fromGregorian 1900 1 1
  maxBound = fromGregorian 9999 1 1

data ConnectingPorts

instance IsAttribute MinMCT ConnectingPorts where
  type Option MinMCT ConnectingPorts = OptionEnum TransitPorts
  maybeOption _ k = Just . MkOptionEnum $ rAirports (options k)

data ArrivalTerminal

instance IsAttribute MinMCT ArrivalTerminal where
  type Option MinMCT ArrivalTerminal = OptionEnum Terminal
  maybeOption _ k = MkOptionEnum <$> rArrTerminal (options k)
  
data DepartureTerminal

instance IsAttribute MinMCT DepartureTerminal where
  type Option MinMCT DepartureTerminal = OptionEnum Terminal
  maybeOption _ k = MkOptionEnum <$> rDepTerminal (options k)

data TransitStatus

instance IsAttribute MinMCT TransitStatus where
  type Option MinMCT TransitStatus = OptionEnum TransitFlow
  maybeOption _ k = Just . MkOptionEnum $ rTransitFlow (options k)

data ArrivalCarrier

instance IsAttribute MinMCT ArrivalCarrier where
  type Option MinMCT ArrivalCarrier = OptionEnum AirlineCode
  maybeOption _ k = MkOptionEnum <$> rArrCarrier (options k)

data DepartureCarrier

instance IsAttribute MinMCT DepartureCarrier where
  type Option MinMCT DepartureCarrier = OptionEnum AirlineCode
  maybeOption _ k = MkOptionEnum <$> rDepCarrier (options k)

data PreviousRegion

instance IsAttribute MinMCT PreviousRegion where
  type Option MinMCT PreviousRegion = OptionEnum Region
  maybeOption _ k = MkOptionEnum <$> rPrevRegion (options k)

data NextRegion

instance IsAttribute MinMCT NextRegion where
  type Option MinMCT NextRegion = OptionEnum Region
  maybeOption _ k = MkOptionEnum <$> rNextRegion (options k)

data PreviousCountry

instance IsAttribute MinMCT PreviousCountry where
  type Option MinMCT PreviousCountry = OptionEnum Country
  maybeOption _ k = MkOptionEnum <$> rPrevCountry (options k)

data NextCountry

instance IsAttribute MinMCT NextCountry where
  type Option MinMCT NextCountry = OptionEnum Country
  maybeOption _ k = MkOptionEnum <$> rNextCountry (options k)

data PreviousState

instance IsAttribute MinMCT PreviousState where
  type Option MinMCT PreviousState = OptionEnum State
  maybeOption _ k = MkOptionEnum <$> rPrevState (options k)

data NextState

instance IsAttribute MinMCT NextState where
  type Option MinMCT NextState = OptionEnum State
  maybeOption _ k = MkOptionEnum <$> rNextState (options k)

data PreviousCity

instance IsAttribute MinMCT PreviousCity where
  type Option MinMCT PreviousCity = OptionEnum City
  maybeOption _ k = MkOptionEnum <$> rPrevCity (options k)

data NextCity

instance IsAttribute MinMCT NextCity where
  type Option MinMCT NextCity = OptionEnum City
  maybeOption _ k = MkOptionEnum <$> rNextCity (options k)

data PreviousAirport

instance IsAttribute MinMCT PreviousAirport where
  type Option MinMCT PreviousAirport = OptionEnum Port
  maybeOption _ k = MkOptionEnum <$> rPrevPort (options k)

data NextAirport

instance IsAttribute MinMCT NextAirport where
  type Option MinMCT NextAirport = OptionEnum Port
  maybeOption _ k = MkOptionEnum <$> rNextPort (options k)

data ArrivalFlightRange

instance IsAttribute MinMCT ArrivalFlightRange where
  type Option MinMCT ArrivalFlightRange = PM.Interval Int
  maybeOption _ k = uncurry PM.ClosedInterval <$> rArrFlights (options k)

data DepartureFlightRange

instance IsAttribute MinMCT DepartureFlightRange where
  type Option MinMCT DepartureFlightRange = PM.Interval Int
  maybeOption _ k = uncurry PM.ClosedInterval <$> rDepFlights (options k)

data ArrivalAircraftBody

instance IsAttribute MinMCT ArrivalAircraftBody where
  type Option MinMCT ArrivalAircraftBody = OptionEnum AircraftBody
  maybeOption _ k = MkOptionEnum <$> rArrAircraftBody (options k)

data DepartureAircraftBody

instance IsAttribute MinMCT DepartureAircraftBody where
  type Option MinMCT DepartureAircraftBody = OptionEnum AircraftBody
  maybeOption _ k = MkOptionEnum <$> rDepAircraftBody (options k)

data ArrivalAircraftType

instance IsAttribute MinMCT ArrivalAircraftType where
  type Option MinMCT ArrivalAircraftType = OptionEnum AircraftType
  maybeOption _ k = MkOptionEnum <$> rArrAircraftType (options k)

data DepartureAircraftType

instance IsAttribute MinMCT DepartureAircraftType where
  type Option MinMCT DepartureAircraftType = OptionEnum AircraftType
  maybeOption _ k = MkOptionEnum <$> rDepAircraftType (options k)

data ValidityPeriod

instance IsAttribute MinMCT ValidityPeriod where
  type Option MinMCT ValidityPeriod = PM.Interval Day
  
  maybeOption _ k = let o = options k
                    in case (rValidityBegin o, rValidityEnd o) of
                       (Nothing, Nothing) -> Nothing
                       (Nothing, Just b ) -> Just $ PM.ClosedInterval minBound b
                       (Just a , Nothing) -> Just $ PM.ClosedInterval a maxBound
                       (Just a , Just b ) -> Just $ PM.ClosedInterval a b

-- | Structure of MCT attributes.
attributes :: [MCTStorable]
attributes = [
               MkStorable $ empty (undefined::ConnectingPorts)
             , MkStorable $ empty (undefined::ArrivalTerminal)
             , MkStorable $ empty (undefined::DepartureTerminal)
             , MkStorable $ empty (undefined::TransitStatus)
             , MkStorable $ empty (undefined::ArrivalCarrier)
             , MkStorable $ empty (undefined::DepartureCarrier)
             , MkStorable $ empty (undefined::PreviousRegion)
             , MkStorable $ empty (undefined::NextRegion)
             , MkStorable $ empty (undefined::PreviousCountry)
             , MkStorable $ empty (undefined::NextCountry)
             , MkStorable $ empty (undefined::PreviousState)
             , MkStorable $ empty (undefined::NextState)
             , MkStorable $ empty (undefined::PreviousCity)
             , MkStorable $ empty (undefined::NextCity)
             , MkStorable $ empty (undefined::PreviousAirport)
             , MkStorable $ empty (undefined::NextAirport)
             , MkStorable $ empty (undefined::ArrivalFlightRange)
             , MkStorable $ empty (undefined::DepartureFlightRange)
             , MkStorable $ empty (undefined::ArrivalAircraftBody)
             , MkStorable $ empty (undefined::DepartureAircraftBody)
             , MkStorable $ empty (undefined::ArrivalAircraftType)
             , MkStorable $ empty (undefined::DepartureAircraftType)
             , MkStorable $ empty (undefined::ValidityPeriod)
             ]
