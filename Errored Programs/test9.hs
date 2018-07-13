{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

data None = None { _f :: Int }

type Simpl = Env

makeLenses ''None

type Env = Int
