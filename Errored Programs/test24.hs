{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances     #-}
data A x = A deriving (Show)
class C y where get :: y
instance (C (A (A a))) => C (A a) where
    get = A

main = print (get :: A ())
