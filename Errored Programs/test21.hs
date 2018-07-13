{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module SGenerics where

import Data.Kind (Type)

type family R :: Type
data family Sing (z :: R)
type family PFrom (x :: Type) :: R

u :: forall (a :: R). Sing a
u = u

v :: forall (a :: Type). Sing (PFrom a)
v = u
