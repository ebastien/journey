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

-- | A traffic restriction.
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

-- | Is traffic restriction lifted on connection.
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

-- | Is the restriction qualified for online connections.
isOnlineQualified :: Restriction -> Bool
isOnlineQualified r = r == QIntlOnlineCnxStop ||
                      r == QOnlineCnxStop ||
                      r == QOnlineCnx

-- | Board and/or off points restriction qualifier
type RestrictQualifier = (Bool, Bool)

-- | A boarding or disembarkation transfer.
data Transfer c = Transfer { cCarrier :: c
                           , cIntl :: Bool
                           , cRestriction :: Restriction
                           } deriving (Eq, Show)

-- | A non-connected trip.
data Local c = Local { lQual :: RestrictQualifier
                     , lCnx :: Transfer c
                     } deriving (Eq, Show)

-- | An aggregated trip.
data Trip c = Trip { tQual :: RestrictQualifier
                   , tQualFlag :: Bool
                   , tInbound :: Transfer c
                   , tOutbound :: Transfer c
                   } deriving (Eq, Show)

-- | Initiate a trip.
initiate :: Local c -> Maybe (Trip c)
initiate (Local (q,p) c) = if not q' || l
                             then Just $ Trip (q',p') f c c
                             else Nothing
  where f = isOnlineQualified r && (p || not q)
        q' = q || not p && l
        p' = p || not (q || l)
        l = isFinal r
        r = cRestriction c

-- | Connect a segment to a trip.
connect :: Eq c => Local c -> Trip c -> Maybe (Trip c)
connect (Local (ql,pl) yl) (Trip (q,p) f x y) =
  if (not p || lO) &&   -- No outbound restriction or lifted
     (not ql || lI) &&  -- No inbound restriction or lifted
     (not f || o)       -- Not fully online qualified or online
    then Just $ Trip (q,p') f' x yl
    else Nothing
  where p' = pl || not (ql || lI)
        Transfer c t r = y
        Transfer cl tl rl = yl
        o = c == cl
        lO = isConnectable r o tl
        lI = isConnectable rl o t
        f' = isOnlineQualified rl && (
               o && f && (ql || not pl) ||
               not o && (pl || not ql) )

-- | Finalize a trip.
finalize :: Trip c -> Maybe (Trip c)
finalize t@(Trip (q,p) f x y) = if (not p || l) && not f
                                  then Just t
                                  else Nothing
  where l = isFinal $ cRestriction y

data RestrictService = MkRestrictService { rPax   :: !Restriction
                                         , rCargo :: !Restriction
                                         , rMail  :: !Restriction
                                         } deriving (Show, Eq)

-- | The monoid of traffic restriction under aggregation.
instance Monoid RestrictService where
  mempty = mkRestrictNone
  mappend (MkRestrictService a b c) (MkRestrictService x y z) =
    MkRestrictService (m a x) (m b y) (m c z)
    where m NoRestriction a = a
          m a NoRestriction = a
          m _ _ = error "Traffic restriction overlap"

-- | Unrestricted services.
mkRestrictNone :: RestrictService
mkRestrictNone = MkRestrictService NoRestriction NoRestriction NoRestriction

-- | Pax service restricted.
mkRestrictPax :: Restriction ->  RestrictService
mkRestrictPax r = MkRestrictService r NoRestriction NoRestriction

-- | Cargo service restricted.
mkRestrictCargo :: Restriction ->  RestrictService
mkRestrictCargo r = MkRestrictService NoRestriction r NoRestriction

-- | Mail service restricted.
mkRestrictMail :: Restriction ->  RestrictService
mkRestrictMail r = MkRestrictService NoRestriction NoRestriction r

-- | Cargo/Mail services restricted.
mkRestrictCargoMail :: Restriction ->  RestrictService
mkRestrictCargoMail r = MkRestrictService NoRestriction r r

-- | All services restricted.
mkRestrictAll :: Restriction ->  RestrictService
mkRestrictAll r = MkRestrictService r r r

-- | Is local traffic allowed.
isLocalAllowed :: RestrictService -> Bool
isLocalAllowed s = not $ r == NoLocal || r == NoDisplay || r == TechnicalLanding
  where r = rPax s
