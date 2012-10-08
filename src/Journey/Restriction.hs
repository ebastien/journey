module Journey.Restriction (
    Restriction(..)
  , paxAllowed
  , Local(..)
  , Traffic(..)
  , initiate
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

isQualified :: Restriction -> Bool
isQualified r = r == QIntlOnlineCnxStop ||
                r == QOnlineCnxStop ||
                r == QOnlineCnx

data Local c = Local { lCarrier :: c
                     , lIntl :: Bool
                     , lRestriction :: Restriction
                     , lBoardQual :: Bool
                     , lOffQual :: Bool
                     } deriving (Eq, Show)

data Cnx c = Cnx { cCarrier :: c
                 , cIntl :: Bool
                 , cRestriction :: Restriction
                 } deriving (Eq, Show)

data Traffic c = Traffic { tInCnx :: Cnx c
                         , tInQual :: Bool
                         , tOutCnx :: Cnx c
                         , tOutQual :: Bool
                         , tQFlag :: Bool
                         } deriving (Eq, Show)

initiate :: Local c -> Maybe (Traffic c)
initiate (Local c t r q p) = Just $ Traffic cnx q cnx p f
  where cnx = Cnx c t r
        f = isQualified r && (p || not q)

connect :: Eq c => Local c -> Traffic c -> Maybe (Traffic c)
connect (Local c2 t2 r2 q2 p2)
        (Traffic x1 q1 y1 p1 f) =
  if (not p1 || p1 && l1) &&
     (not q2 || q2 && l2)
    then Just $ Traffic x1 q (Cnx c2 t2 r2) p f'
    else Nothing
  where q = q1 || not (p1 || l1)
        p = p2 || not (q2 || l2)
        (Cnx c1 t1 r1) = y1
        o = c1 == c2
        l1 = isCnxAllowed r1 o t2
        l2 = isCnxAllowed r2 o t1
        f' = f && isQualified r2 && (q2 || not p2)

complete :: Traffic c -> Maybe (Traffic c)
complete t = if not (tInQual t || tOutQual t || tQFlag t)
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
