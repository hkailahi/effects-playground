module Eff.Types where

-- import BasicPrelude

-- import Control.Eff (Eff)


-- instance Monad (Eﬀ r)

-- Pure computations
-- data Void
-- run :: Eff Void w -> w

-- Reader (or environment) eﬀect
-- type Reader e
-- ask       :: (Typeable e, Member (Reader e) r) => Eﬀ r e
-- local     :: (Typeable e, Member (Reader e) r) => (e -> e) -> Eﬀ r w -> Eﬀ r w
-- runReader :: Typeable e                        => Eﬀ (Reader e B r) w -> e -> Eﬀ r w

-- Exceptions
-- type Exc e
-- throwError :: (Typeable e, Member (Exc e) r) => e -> Eﬀ r a
-- catchError :: (Typeable e, Member (Exc e) r) => Eﬀ r w -> (e -> Eﬀ r w) -> Eﬀ r w
-- runError :: Typeable e                       => Eﬀ (Exc e B r) w -> Eﬀ r (Either e w)

-- State
-- type State s
-- get      :: (Typeable s, Member (State s) r) => Eﬀ r s
-- put      :: (Typeable s, Member (State s) r) => s -> Eﬀ r ()
-- runState :: Typeable s                       => Eﬀ (State s B r) w -> s -> Eﬀ r (w,s)

-- Non−determinism
-- type Choose
-- choose     :: Member Choose r => [w] -> Eﬀ r w
-- makeChoice :: Eﬀ (Choose B r) w -> Eﬀ r [ w]

-- Tracing
-- type Trace
-- trace    :: Member Trace r => String -> Eﬀ r ()
-- runTrace :: Eﬀ (Trace B Void) w -> IO w

-- Built−in eﬀects (e. g., IO)
-- type Lift m
-- lift    :: (Typeable1 m, MemberU2 Lift (Lift m) r) => m w -> Eﬀ r w
-- runLift :: (Monad m, Typeable1 m)                  => Eﬀ (Lift m B Void) w -> m w