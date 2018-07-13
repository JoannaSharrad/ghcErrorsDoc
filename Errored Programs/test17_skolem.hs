{-# LANGUAGE RankNTypes #-}

data Foo a

type A a = forall m. Monad m => Foo a -> m ()
type PA a = forall m. Monad m => Foo a -> m ()
type PPFA a = forall m. Monad m => Foo a -> m ()

_pfa :: PPFA a -> PA a
_pfa = _pfa

_pa :: PA a -> A a
_pa = _pa

_pp :: PPFA a -> A a
_pp = _pa . _pfa

main :: IO ()
main = putStrLn "yay"
