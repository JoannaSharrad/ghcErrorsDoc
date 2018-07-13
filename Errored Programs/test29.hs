{-# LANGUAGE PolyKinds, RankNTypes, ConstraintKinds, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Foo where
class Class1 b h | h -> b
instance Class1 Functor Applicative
instance Class1 Applicative Monad

class SuperClass1 b h
instance {-# OVERLAPPING #-} SuperClass1 b b
instance {-# OVERLAPPABLE #-} (SuperClass1 b c, Class1 c h) => SuperClass1 b h

newtype HFree c f a = HFree { runHFree :: forall g. c g => (forall b. f b -> g b) -> g a }

instance SuperClass1 Functor c => Functor (HFree c f)
instance SuperClass1 Applicative c => Applicative (HFree c f)
instance SuperClass1 Monad c => Monad (HFree c f)

test :: (a -> b) -> HFree Monad f a -> HFree Monad f b
test = fmap
