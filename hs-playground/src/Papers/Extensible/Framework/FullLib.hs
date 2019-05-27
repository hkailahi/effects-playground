{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Papers.Extensible.Framework.FullLib where

import BasicPrelude
import Data.Typeable ()
import Data.Void

import Papers.Extensible.Framework.OpenUnions

---------------------------------------------------------------------------------------------------

data VE w r
  = Val w
  | E (Union r (VE w r))

---------------------------------------

newtype Eff r a = Eff
  { runEff :: forall w. (a -> VE w r) -> VE w r}

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
-- (a return address of type `a -> VE w r`), passes `k` to the user-speciﬁed request builder `f` obtaining
-- the request body (of the type `r (VE w r)`), incorporates it into the request `E`, and delivers it to
-- the waiting `admin`
send :: (forall w. (a -> VE w r) -> Union r (VE w r)) -> Eff r a
send f = Eff $ \k -> E (f k)

-- |Partial, but shouldn't have other case. Maybe worth adding refinement
admin :: Eff r w -> VE w r
admin (Eff m) = m Val

run :: Eff Void w -> w
run m =
  case admin m of
    Val x -> x
    _     -> error "Refine me"

---------------------------------------
-- Helpers to relay unrecognized requests

handle_relay :: Typeable t
  => Union (t |> r) v
  -> (v -> Eff r a)
  -> (t v -> Eff r a)
  -> Eff r a
handle_relay union' loop handler =
  case decomp union' of
    Right x -> handler x
    Left  y -> do
      resp <- send $ \k -> fmap k y
      loop resp

interpose :: (Typeable t)
  => Union r v
  -> (v -> Eff r a)
  -> (t v -> Eff r a)
  -> Eff r a
interpose union' loop handler =
  case prj union' of
    Just x -> handler x
    _      -> do
      resp <- send $ \k -> fmap k union'
      loop resp

---------------------------------------
-- Reader Effect

newtype Reader e v =
  Reader (e -> v)
  deriving (Typeable, Functor)

ask :: (Typeable e) => Eff r e
ask = send $ inj . Reader


-- |The return type shows that all `Reader e` requests are fully handled.
-- The `runReader` computation may have other requests, though, represented by `r`.
-- The `runReader` handler, as before, obtains the status of the client computation using
-- `admin` and analyzes it, handling three possible cases:
--
-- 1. If the client completed, its result is returned.
-- 2. If the client sent a request, we check if it is a `Reader` request. If so, the client is
-- resumed with the current value of the dynamic environment. The client may then terminate
-- or send another request, hence we `loop`.
-- 3. If the request is not a `Reader` request, we re-send it. That other request, `u`, must
-- have contained the return address, the suspension of the type `t → VE w (Reader e |> r)`.
-- When re-sending the request, `runReader` obtains its own suspension `k`, which has the type
-- `VE w (Reader e |> r) → VE w’ r`. We must somehow compose the two suspensions, to obtain
-- `t → VE w’ r`. When `runReader`’s handler resumes it, the `runReader`’s client is resumed.
runReader :: Typeable e => Eff (Reader e |> r) w -> e -> Eff r w
runReader m e = loop (admin m)
  where loop (Val x) = pure x
        loop (E u)   =
          handle_relay u loop
            $ \(Reader k) ->
              loop $ k e

local :: forall a e r. (Typeable e) => (e -> e) -> Eff r a -> Eff r a
local f m = do
  oldEnv <- ask
  let newEnv :: e
      newEnv = f oldEnv
      loop :: VE a r -> Eff r a
      loop (Val x) = pure x
      loop (E u)   =
        interpose u loop
          $ \(Reader k) ->
            loop $ k newEnv
  loop (admin m)

---------------------------------------
-- Exceptions

newtype Exc e v =
  Exc e
  deriving (Functor, Typeable)

-- throwError :: (Typeable e, Member (Exc e) r) => e -> Eff r a
throwError :: (Typeable e) => e -> Eff r a
throwError e = send $ \_ -> inj $ Exc e

-- runError ::  Typeable e -> Eff (Exc e |> r) a -> Eff r (Either e a)
-- catchError :: (Typeable e, Member (Exc e) r) -> Eff r a -> (e -> Eff r a) -> Eff r a
