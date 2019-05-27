{-# Language DeriveFunctor #-}

module Papers.Extensible.Framework.OpenUnions where

import BasicPrelude

import Data.Typeable

---------------------------------------------------------------------------------------------------

infixr 1 |>
data ((a :: * -> *) |> b)

-- class Member (t :: * -> *) r where
--   inj :: (Functor t, Member t r) => t v -> Union r v
--   prj :: (Functor t, Member t r) => Union r v -> Maybe (t v)
--   decomp :: Union (t |> r) v -> Either (Union r v) (t v)

-- inj :: (Functor t, Member t r) => t v -> Union r v prj :: (Functor t, Member t r) => Union r v -> Maybe (t v)
-- decomp :: Union (t |> r) v -> Either (Union r v) (t v)


---------------------------------------------------------------------------------------------------
-- Open Unions - HList Implementation

newtype Id x = Id x -- for the sake of `gcast1`

-- |It's `Dynamic`
data Union r v where
  Union :: (Functor t, Typeable t) => Id (t v) -> Union r v

instance Functor (Union r) where
  fmap f (Union (Id x)) = Union . Id $ fmap f x

-- |The injection takes a request of type `t` and adds it to the union `r`.
-- The constraint `Member t r` ensures that `t` participates in the union.
-- inj :: (Functor t, Typeable t, Member t r) => t v -> Union r v
inj :: (Functor t, Typeable t) => t v -> Union r v
inj x = Union $ Id x

-- |The projection does the opposite of injection. Given a value of type `Union (t |> r)` that may have
-- a summand of the type `t`, the orthogonal decomposition decomp determines if the value has that
-- request type `t. If it does, it is returned.
-- Otherwise, the union value is cast to a more restrictive `Union r` type without `t` — we have just
-- determined the value is not of type `t`.
-- prj :: (Functor t, Typeable t, Member t r) => Union r v -> Maybe (t v)
prj :: (Typeable t) => Union r v -> Maybe (t v)
prj (Union v) | Just (Id x) <- gcast1 v = Just x
prj _                                   = Nothing

-- Projects `Union r` into two orthogonal “spaces:” one for the particular type `t` and the other for `r`
-- without `t`. This operation sets our open unions apart from previous designs: we can, not only extend unions,
-- but also shrink them. The decomposition also distinguishes our open unions from the extensible polymorphic
-- variants of OCaml.
decomp :: Typeable t => Union (t |> r) v -> Either (Union r v) (t v)
decomp (Union v) | Just (Id x) <- gcast1 v = Right x
decomp (Union v)                           = Left $ Union v

class Member (t :: * -> * ) r

instance Member t (t |> r)