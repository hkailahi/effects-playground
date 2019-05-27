module Papers.Extensible.Framework.Reader where

import BasicPrelude

---------------------------------------------------------------------------------------------------
-- Client <-> Effect Handler Coroutine as the Continuation Monad

---------------------------------------

-- |The Value-Effect datatype
data VE w
  = Val w
  | E (Int -> VE w)

-- |The type of computations that perform control effects instantiated to answer types (`VE w`) for
-- polymorphic `w` (short for Value-Effect, indicating the two types that make up the signature).
newtype Eff a = Eff
  { runEff :: forall w. (a -> VE w) -> VE w
    -- ^ Computation that may produce a value (`Val w`) or send a request to read the `Int` environment.
    -- This request, when resumed, continues the computation, which then recursively produces another
    -- answer of type (`VE w`) (or diverges)
  }

instance Functor Eff where
  fmap f (Eff x) = Eff $ \x' -> x (x' . f)

instance Applicative Eff where
  pure x = Eff $ \k -> k x
  Eff f <*> Eff a = Eff $ \br ->
    f $ \ab ->
      a $ br . ab

instance Monad Eff where
  return   = pure
  m >>= f  = Eff $ \k ->
    runEff m (\v -> runEff (f v) k)

---------------------------------------

-- |Sends a request that retrieves the current value from the environment as follows: Obtains
-- the current continuation (the ‘return address’) and incorporates it into the request,
-- constructing the `Int → VE w` function that will be invoked by `runReader` to produce the
-- final answer.
ask :: Eff Int
ask = Eff $ \k -> E k

-- |Runs a computation in a changed environment
-- On one hand, `local` must handle `Reader` requests, similar to `runReader`, and on the other,
-- `local` must send `Reader` requests to obtain the environment value to modify. As a result, the
-- type of `local`, unlike the type of `runReader`, does not promise to remove the `Reader` effect.
local :: (Int -> Int) -> Eff w -> Eff w
local f m = do
  oldEnv <- ask
  let newEnv :: Int
      newEnv = f oldEnv
      loop :: VE w -> Eff w
      loop (Val x) = pure x
      loop (E k)   = loop $ k newEnv
  loop (admin m)

-- |Launches a coroutine with an initial continuation expecting a value, which,
-- unless the computation diverges, must be the ultimate result.
admin :: Eff w -> VE w
admin (Eff m) = m Val

-- |Handler that launches the coroutine and checks its status. If the coroutine sends an answer,
-- the result is returned. If the coroutine sends a request asking for the current value of the
-- environment, that value `e` is given in reply.
runReader :: Eff w -> Int -> w
runReader m e = loop $ admin m
  where
    loop :: VE w -> w
    loop (Val x) = x
    loop (E k)   = loop (k e)

---------------------------------------------------------------------------------------------------

-- |The status type for such exception-throwing coroutines
data VEex w = ValEx w | EEx Bool

-- |To non-deterministically choosing an element from a given list, send the request that includes
-- the list and the return address expecting one element in reply
data VEch w = ValEc w | forall a. EEc [a] (a -> VEch w)
