{-# LANGUAGE RankNTypes #-}

data Foo a

type A a = forall m. Monad m => Foo a -> m ()
type PA a = forall m. Monad m => Foo a -> m ()
type PPFA a = forall m. Monad m => Foo a -> m ()

_pfa :: PPFA a -> PA a
_pfa = undefined

_pa :: PA a -> A a
_pa = undefined

_pp :: PPFA a -> A a
_pp = undefined

main :: IO ()
main = putStrLn "yay"
