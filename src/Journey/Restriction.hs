module Journey.Restriction (
    Restriction(..)
  , paxAllowed
  , RestrictService(..)
  , RestrictQualifier(..)
  , mkRestrictNone
  , mkRestrictPax
  , mkRestrictCargo
  , mkRestrictMail
  , mkRestrictCargoMail
  , mkRestrictAll
  ) where

import Data.Monoid (Monoid(..))

data Restriction = NoRestriction
                 | NoDirect
                 | NoConnection
                 | NoInternational
                 | QIntlOnlineCnxStop
                 | QOnlineCnxStop
                 | NoInterline
                 | QOnlineCnx
                 | NoDisplay
                 | TechnicalLanding
                 | Connection
                 | IntlOnlineStop
                 | IntlConnection
                 | IntlOnlineCnx
                 | IntlOnlineCnxStop
                 | OnlineStop
                 | ConnectionStop
                 | IntlCnxStop
                 | OnlineCnxStop
                 | OnlineCnx
                 deriving (Show, Eq)

serviceDenied = [ NoDirect, NoDisplay, TechnicalLanding ]

paxAllowed :: RestrictService -> Bool
paxAllowed r = not $ rPax r `elem` serviceDenied

data RestrictService = MkRestrictService { rPax   :: !Restriction
                                         , rCargo :: !Restriction
                                         , rMail  :: !Restriction
                                         } deriving (Show)

instance Monoid RestrictService where
  mempty = mkRestrictNone
  mappend (MkRestrictService a b c) (MkRestrictService x y z) =
    MkRestrictService (m a x) (m b y) (m c z)
    where m NoRestriction a = a
          m a NoRestriction = a
          m _ _ = error "Traffic restriction overlap"

mkRestrictNone :: RestrictService
mkRestrictNone = MkRestrictService NoRestriction NoRestriction NoRestriction

mkRestrictPax :: Restriction ->  RestrictService
mkRestrictPax r = MkRestrictService r NoRestriction NoRestriction

mkRestrictCargo :: Restriction ->  RestrictService
mkRestrictCargo r = MkRestrictService NoRestriction r NoRestriction

mkRestrictMail :: Restriction ->  RestrictService
mkRestrictMail r = MkRestrictService NoRestriction NoRestriction r

mkRestrictCargoMail :: Restriction ->  RestrictService
mkRestrictCargoMail r = MkRestrictService NoRestriction r r

mkRestrictAll :: Restriction ->  RestrictService
mkRestrictAll r = MkRestrictService r r r

data RestrictQualifier = RestrictBoard
                       | RestrictOff
                       | RestrictBoardOff
                       deriving (Show)
