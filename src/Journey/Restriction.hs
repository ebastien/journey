module Journey.Restriction (
    Restriction(..)
  , isLocalAllowed
  , Cnx(..)
  , Local(..)
  , Trip(..)
  , initiate
  , connect
  , finalize
  , RestrictService(..)
  , RestrictQualifier
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

-- | Is local traffic allowed.
isLocalAllowed :: RestrictService -> Bool
isLocalAllowed r = not $ rPax r `elem` [ NoLocal, NoDisplay, TechnicalLanding ]

-- | Is the restriction qualified for online connections.
isQualified :: Cnx c -> Bool
isQualified c = r == QIntlOnlineCnxStop ||
                r == QOnlineCnxStop ||
                r == QOnlineCnx
  where r = cRestriction c

type RestrictQualifier = (Bool, Bool)

data Cnx c = Cnx { cCarrier :: c
                 , cIntl :: Bool
                 , cRestriction :: Restriction
                 } deriving (Eq, Show)

data Local c = Local { lQual :: RestrictQualifier
                     , lCnx :: Cnx c
                     } deriving (Eq, Show)

data Trip c = Trip { tQual :: RestrictQualifier
                   , tQualFlag :: Bool
                   , tInbound :: Cnx c
                   , tOutbound :: Cnx c
                   } deriving (Eq, Show)

-- | Initiate a trip.
initiate :: Local c -> Maybe (Trip c)
initiate (Local (q,p) c) = Just $ Trip (q,p) f c c
  where f = isQualified c && (p || not q)

-- | Finalize a trip.
finalize :: Trip c -> Maybe (Trip c)
finalize t@(Trip (q,p) f _ _) = if not (q || p || f)
                                  then Just t
                                  else Nothing

-- | Connect a segment to a trip.
connect :: Eq c => Local c -> Trip c -> Maybe (Trip c)
connect (Local (q2,p2) y2)
        (Trip (q1,p1) f x1 y1) =
  if (not p1 || p1 && l1) &&
     (not q2 || q2 && l2)
    then Just $ Trip (q,p) f' x1 y2
    else Nothing
  where q = q1 || not (p1 || l1)
        p = p2 || not (q2 || l2)
        (Cnx c1 t1 r1) = y1
        (Cnx c2 t2 r2) = y2
        o = c1 == c2
        l1 = isCnxAllowed r1 o t2
        l2 = isCnxAllowed r2 o t1
        f' = f && isQualified y2 && (q2 || not p2)

-- | Is construction of transfer connection allowed.
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

