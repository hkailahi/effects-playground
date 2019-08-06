module Papers.Extensible.Framework.ArbEffects where

import BasicPrelude

---------------------------------------------------------------------------------------------------

-- |It's `Free`
data VE w r
  = Val w
  | E (r (VE w r))

-- |`Eff` generalized for arbitrary effects
-- The coroutine monad is indexed by the type of requests `r` that the coroutine may send
newtype Eff r a = Eff
  { runEff :: forall w. (a -> VE w r) -> VE w r }

instance Functor (Eff r) where
  fmap f (Eff x) = Eff $ \x' -> x (x' . f)

instance Applicative (Eff r) where
  pure x = Eff $ \k -> k x
  Eff f <*> Eff a = Eff $ \br ->
    f $ \ab ->
      a $ br . ab

instance Monad (Eff r) where
  return   = pure
  m >>= f  = Eff $ \k ->
    runEff m (\v -> runEff (f v) k)

-- |Dispatches requests and waits for a reply. It obtains the suspension `k` of the current computation
-- (a return address of type `a → VE w r`), passes `k` to the user-speciﬁed request builder `f` obtaining
-- the request body (of the type `r (VE w r)`), incorporates it into the request `E`, and delivers it to
-- the waiting `admin`
send :: (forall w. (a -> VE w r) -> r (VE w r)) -> Eff r a
send f = Eff $ \k -> E (f k)

-- |Partial, but shouldn't have other case. Maybe worth adding refinement
admin :: Eff r w -> VE w r
admin (Eff m) = m Val

---------------------------------------------------------------------------------------------------
-- Example Effects

---------------------------------------
-- Pure Computation

-- No Inhabitants
data Void v

run :: Eff Void w -> w
run m =
  case admin m of
    Val x -> x
    _     -> error "Can't. Doesn't make sense to be here"

---------------------------------------
-- Reading from Environment

newtype Reader e v = Reader (e -> v)

ask :: Eff (Reader e) e
ask = send Reader

-- |The signature of indicates that it takes a computation that may send (`Reader e`) requests and
-- completely handles them. The result is the pure computation with nothing left unhandled.
runReader :: forall e w. Eff (Reader e) w -> e -> Eff Void w
runReader m e = loop $ admin m
  where
    loop :: VE w (Reader e) -> Eff Void w
    loop (Val x)        = pure x
    loop (E (Reader k)) = loop (k e)

add :: (Applicative f, Num x) => f x -> f x -> f x
add = liftA2 (+)

-- |Effectful computation that send a requests to retrieve an `Int` from environment and increment reponse
t1 :: Eff (Reader Int) Int
t1 = ask `add` pure (1 :: Int)

-- >>> run $ t1r
-- 11
t1r :: Eff Void Int
t1r = runReader t1 10
