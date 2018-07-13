{-# LANGUAGE PatternSynonyms #-}
module Foo where
pattern Pat :: () => Show a => a -> Maybe a
pattern Pat a = Just a

