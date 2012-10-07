module Journey.Restriction (
    Restriction(..)
  , paxAllowed
  , Cnx(..)
  , Traffic(..)
  , connect
  , complete
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
                 | NoLocal
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

type Qualifier = (Bool, Bool)

serviceDenied = [ NoLocal, NoDisplay, TechnicalLanding ]

paxAllowed :: RestrictService -> Bool
paxAllowed r = not $ rPax r `elem` serviceDenied

data Cnx c = Cnx c Bool Restriction
             deriving (Eq, Show)
data Traffic c = Traffic (Cnx c) (Cnx c) Qualifier
                 deriving (Eq, Show)

connect :: Eq c => Traffic c -> Traffic c -> Maybe (Traffic c)
connect (Traffic x1 y1 (q1, p1))
        (Traffic x2 y2 (q2, p2)) =
  if (not p1 || p1 && l1) &&
     (not q2 || q2 && l2)
    then Just $ Traffic x1 y2 q
    else Nothing
  where q = ( q1 || not (p1 || l1)
            , p2 || not (q2 || l2) )
        (Cnx a1 t1 r1) = y1
        (Cnx a2 t2 r2) = y2
        o = a1 == a2
        l1 = isCnxAllowed r1 o t2
        l2 = isCnxAllowed r2 o t1

complete :: Traffic c -> Maybe (Traffic c)
complete t@(Traffic _ _ (q, p)) = if not (q || p)
                                    then Just t
                                    else Nothing

isCnxAllowed :: Restriction -> Bool -> Bool -> Bool
isCnxAllowed r o t = case r of
  NoRestriction       -> True
  NoLocal             -> False
  NoConnection        -> False
  NoInternational     -> not t
  QIntlOnlineCnxStop  -> o && t
  QOnlineCnxStop      -> o
  NoInterline         -> o
  QOnlineCnx          -> o
  NoDisplay           -> False
  TechnicalLanding    -> False
  Connection          -> True
  IntlOnlineStop      -> False
  IntlConnection      -> t
  IntlOnlineCnx       -> o && t
  IntlOnlineCnxStop   -> o && t
  OnlineStop          -> False
  ConnectionStop      -> True
  IntlCnxStop         -> t
  OnlineCnxStop       -> o
  OnlineCnx           -> o

data RestrictService = MkRestrictService { rPax   :: !Restriction
                                         , rCargo :: !Restriction
                                         , rMail  :: !Restriction
                                         } deriving (Show, Eq)

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
                       | RestrictAny
                       deriving (Show, Eq)
