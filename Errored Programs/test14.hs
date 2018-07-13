{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Foo where

import GHC.TypeLits (Symbol, Nat, KnownNat, natVal, KnownSymbol, symbolVal)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Proxy (Proxy(..))

data TextMax (n :: Nat) = TextMax Text
  deriving (Show)

textMax :: KnownNat n => Text -> Maybe (TextMax n)
textMax t
  | Text.length t <= (fromIntegral $ natVal (Proxy :: Proxy n)) = Just (TextMax t)
  | otherwise = Nothing
