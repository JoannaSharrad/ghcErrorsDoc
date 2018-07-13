newtype Foo f a = Foo (f (f a)) deriving Eq

