module Journey.Restriction (
    Restriction(..)
  , isLocalAllowed
  , Transfer(..)
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
isQualified :: Transfer c -> Bool
isQualified c = r == QIntlOnlineCnxStop ||
                r == QOnlineCnxStop ||
                r == QOnlineCnx
  where r = cRestriction c

type RestrictQualifier = (Bool, Bool)

data Transfer c = Transfer { cCarrier :: c
                           , cIntl :: Bool
                           , cRestriction :: Restriction
                           } deriving (Eq, Show)

data Local c = Local { lQual :: RestrictQualifier
                     , lCnx :: Transfer c
                     } deriving (Eq, Show)

data Trip c = Trip { tQual :: RestrictQualifier
                   , tQualFlag :: Bool
                   , tInbound :: Transfer c
                   , tOutbound :: Transfer c
                   } deriving (Eq, Show)

-- | Initiate a trip.
initiate :: Local c -> Maybe (Trip c)
initiate (Local (q,p) c) = Just $ Trip (q,p) f c c
  where f = isQualified c && (p || not q)

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
        (Transfer c1 t1 r1) = y1
        (Transfer c2 t2 r2) = y2
        o = c1 == c2
        l1 = isConnectable r1 o t2
        l2 = isConnectable r2 o t1
        f' = f && isQualified y2 && (q2 || not p2)

-- | Is traffic restriction lifted for connection.
isConnectable :: Restriction -> Bool -> Bool -> Bool
isConnectable r o t = case r of
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
  IntlOnlineStop      -> o && t
  IntlConnection      -> t
  IntlOnlineCnx       -> o && t
  IntlOnlineCnxStop   -> o && t
  OnlineStop          -> o
  ConnectionStop      -> True
  IntlCnxStop         -> t
  OnlineCnxStop       -> o
  OnlineCnx           -> o

-- | Finalize a trip.
finalize :: Trip c -> Maybe (Trip c)
finalize t@(Trip (q,p) f x y) = if not (q' || p' || f)
                                  then Just t
                                  else Nothing
  where q' = q || not (p || l1)
        p' = p || not (q || l2)
        l1 = isFinal $ cRestriction x
        l2 = isFinal $ cRestriction y

-- | Is traffic restriction lifted at origin or destination.
isFinal :: Restriction -> Bool
isFinal r = case r of
  NoRestriction       -> True
  NoLocal             -> False
  NoConnection        -> True
  NoInternational     -> True
  QIntlOnlineCnxStop  -> True
  QOnlineCnxStop      -> True
  NoInterline         -> True
  QOnlineCnx          -> False
  NoDisplay           -> False
  TechnicalLanding    -> False
  Connection          -> False
  IntlOnlineStop      -> True
  IntlConnection      -> False
  IntlOnlineCnx       -> False
  IntlOnlineCnxStop   -> True
  OnlineStop          -> True
  ConnectionStop      -> True
  IntlCnxStop         -> True
  OnlineCnxStop       -> True
  OnlineCnx           -> False

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

