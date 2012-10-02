module Journey.Restriction (
    Restriction(..)
  , RestrictService(..)
  , RestrictQualifier(..)
  , mkRestrictAll
  , mkRestrictPax
  , mkRestrictNone
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
                 | ExtRestriction
                 deriving (Show, Eq)

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

mkRestrictAll :: Restriction ->  RestrictService
mkRestrictAll r = MkRestrictService r r r

mkRestrictPax :: Restriction ->  RestrictService
mkRestrictPax r = MkRestrictService r NoRestriction NoRestriction

mkRestrictNone :: RestrictService
mkRestrictNone = MkRestrictService NoRestriction NoRestriction NoRestriction

data RestrictQualifier = RestrictBoard
                       | RestrictOff
                       | RestrictBoardOff
                       deriving (Show)
