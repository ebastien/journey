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
import Journey.Period
import Journey.MCT.DecisionTree
import Journey.MCT.Tree
import Journey.MCT.Rule

data OptionSingleton = OptionIsSet

-- | A container for singleton attributes.
instance IsStore Rule OptionSingleton where
  newtype OptionStore Rule OptionSingleton = MkMCTSingle (Maybe MCTTree)
  emptyStore = MkMCTSingle Nothing
  storeOption (MkMCTSingle s) _ c = case s of
                                      Nothing -> MkMCTSingle $ Just (c Empty)
                                      Just t  -> MkMCTSingle $ Just (c t)
  fetchOption (MkMCTSingle s) _ = maybeToList s

newtype OptionEnum e = MkOptionEnum { optionEnum :: e }

-- | A container for Enum attributes.
instance Enum e => IsStore Rule (OptionEnum e) where
  newtype OptionStore Rule (OptionEnum e) = MkMCTEnum (EM.EnumMap e MCTTree)
  emptyStore = MkMCTEnum $ EM.empty
  storeOption (MkMCTEnum s) o c = let f _ = c
                                  in MkMCTEnum
                                   $ EM.insertWith f (optionEnum o) (c Empty) s
  fetchOption (MkMCTEnum s) o = maybeToList $ EM.lookup (optionEnum o) s

-- | A container for range attributes.
instance Ord o => IsStore Rule (PM.Interval o) where
  newtype OptionStore Rule (PM.Interval o) = MkMCTRange (PM.IntervalMap o MCTTree)
  emptyStore = MkMCTRange $ PM.empty
  storeOption (MkMCTRange s) o c = let f _ = c
                                   in MkMCTRange
                                    $ PM.insertWith f o (c Empty) s
  fetchOption (MkMCTRange s) o = map snd $ PM.intersecting s o

data TransitIntraPort

instance IsAttribute Rule TransitIntraPort where
  type Option Rule TransitIntraPort = OptionSingleton
  maybeOption _ k = if rIntraPort (rOptions k)
                      then Just OptionIsSet
                      else Nothing

data ConnectingPorts

instance IsAttribute Rule ConnectingPorts where
  type Option Rule ConnectingPorts = OptionEnum POnD
  maybeOption _ k = MkOptionEnum <$> fromOption (rAirports $ rOptions k)

data ArrivalTerminal

instance IsAttribute Rule ArrivalTerminal where
  type Option Rule ArrivalTerminal = OptionEnum Terminal
  maybeOption _ k = MkOptionEnum <$> fromOption (rArrTerminal $ rOptions k)
  
data DepartureTerminal

instance IsAttribute Rule DepartureTerminal where
  type Option Rule DepartureTerminal = OptionEnum Terminal
  maybeOption _ k = MkOptionEnum <$> fromOption (rDepTerminal $ rOptions k)

data TransitStatus

instance IsAttribute Rule TransitStatus where
  type Option Rule TransitStatus = OptionEnum TransitFlow
  maybeOption _ k = Just . MkOptionEnum $ rTransitFlow (rOptions k)

data ArrivalCarrier

instance IsAttribute Rule ArrivalCarrier where
  type Option Rule ArrivalCarrier = OptionEnum AirlineCode
  maybeOption _ k = MkOptionEnum <$> fromOption (rArrCarrier $ rOptions k)

data DepartureCarrier

instance IsAttribute Rule DepartureCarrier where
  type Option Rule DepartureCarrier = OptionEnum AirlineCode
  maybeOption _ k = MkOptionEnum <$> fromOption (rDepCarrier $ rOptions k)

data PreviousRegion

instance IsAttribute Rule PreviousRegion where
  type Option Rule PreviousRegion = OptionEnum Region
  maybeOption _ k = MkOptionEnum <$> fromOption (rPrevRegion $ rOptions k)

data NextRegion

instance IsAttribute Rule NextRegion where
  type Option Rule NextRegion = OptionEnum Region
  maybeOption _ k = MkOptionEnum <$> fromOption (rNextRegion $ rOptions k)

data PreviousCountry

instance IsAttribute Rule PreviousCountry where
  type Option Rule PreviousCountry = OptionEnum Country
  maybeOption _ k = MkOptionEnum <$> fromOption (rPrevCountry $ rOptions k)

data NextCountry

instance IsAttribute Rule NextCountry where
  type Option Rule NextCountry = OptionEnum Country
  maybeOption _ k = MkOptionEnum <$> fromOption (rNextCountry $ rOptions k)

data PreviousState

instance IsAttribute Rule PreviousState where
  type Option Rule PreviousState = OptionEnum State
  maybeOption _ k = MkOptionEnum <$> fromOption (rPrevState $ rOptions k)

data NextState

instance IsAttribute Rule NextState where
  type Option Rule NextState = OptionEnum State
  maybeOption _ k = MkOptionEnum <$> fromOption (rNextState $ rOptions k)

data PreviousCity

instance IsAttribute Rule PreviousCity where
  type Option Rule PreviousCity = OptionEnum City
  maybeOption _ k = MkOptionEnum <$> fromOption (rPrevCity $ rOptions k)

data NextCity

instance IsAttribute Rule NextCity where
  type Option Rule NextCity = OptionEnum City
  maybeOption _ k = MkOptionEnum <$> fromOption (rNextCity $ rOptions k)

data PreviousAirport

instance IsAttribute Rule PreviousAirport where
  type Option Rule PreviousAirport = OptionEnum Port
  maybeOption _ k = MkOptionEnum <$> fromOption (rPrevPort $ rOptions k)

data NextAirport

instance IsAttribute Rule NextAirport where
  type Option Rule NextAirport = OptionEnum Port
  maybeOption _ k = MkOptionEnum <$> fromOption (rNextPort $ rOptions k)

data ArrivalFlightRange

fromFlightRange :: FlightRange -> PM.Interval Int
fromFlightRange (MkFlightRange a b) = PM.ClosedInterval a b

instance IsAttribute Rule ArrivalFlightRange where
  type Option Rule ArrivalFlightRange = PM.Interval Int
  maybeOption _ k = fromFlightRange <$> fromOption (rArrFlights $ rOptions k)

data DepartureFlightRange

instance IsAttribute Rule DepartureFlightRange where
  type Option Rule DepartureFlightRange = PM.Interval Int
  maybeOption _ k = fromFlightRange <$> fromOption (rDepFlights $ rOptions k)

data ArrivalAircraftBody

instance IsAttribute Rule ArrivalAircraftBody where
  type Option Rule ArrivalAircraftBody = OptionEnum AircraftBody
  maybeOption _ k = MkOptionEnum <$> fromOption (rArrAircraftBody $ rOptions k)

data DepartureAircraftBody

instance IsAttribute Rule DepartureAircraftBody where
  type Option Rule DepartureAircraftBody = OptionEnum AircraftBody
  maybeOption _ k = MkOptionEnum <$> fromOption (rDepAircraftBody $ rOptions k)

data ArrivalAircraftType

instance IsAttribute Rule ArrivalAircraftType where
  type Option Rule ArrivalAircraftType = OptionEnum AircraftType
  maybeOption _ k = MkOptionEnum <$> fromOption (rArrAircraftType $ rOptions k)

data DepartureAircraftType

instance IsAttribute Rule DepartureAircraftType where
  type Option Rule DepartureAircraftType = OptionEnum AircraftType
  maybeOption _ k = MkOptionEnum <$> fromOption (rDepAircraftType $ rOptions k)

data ValidityPeriod

instance IsAttribute Rule ValidityPeriod where
  type Option Rule ValidityPeriod = PM.Interval Day
  
  maybeOption _ k = let o = rOptions k
                    in case (rValidityBegin o, rValidityEnd o) of
                       (Unknown, Unknown) -> Nothing
                       (Unknown, Known b) -> Just $ PM.ClosedInterval minBound b
                       (Known a, Unknown) -> Just $ PM.ClosedInterval a maxBound
                       (Known a, Known b) -> Just $ PM.ClosedInterval a b

-- | Structure of MCT attributes.
attributes :: [MCTStorable]
attributes = [
               MkStorable $ empty (undefined::TransitIntraPort)
             , MkStorable $ empty (undefined::ConnectingPorts)
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
