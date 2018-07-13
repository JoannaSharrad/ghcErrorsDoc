{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Foo where

import qualified Data.List                  as List
import           Language.Haskell.TH.Syntax         (addTopDecls)

ex9 :: ()
ex9 = cat

$(do
    ds <- [d| f = cab
              cat = ()
            |]
    addTopDecls ds
    [d| g = cab
        cap = True
      |])





