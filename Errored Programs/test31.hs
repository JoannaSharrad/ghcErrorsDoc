{-# LANGUAGE FlexibleInstances #-}
module Style where

import Control.Monad.Writer.Lazy


type StyleM = Writer [(String, String)]
newtype Style = Style { runStyle :: StyleM () }


class Term a where
    term :: String -> a

instance Term String where
    term = id

instance Term (String -> StyleM ()) where
    term property value = tell [(property, value)]


display :: String -> StyleM ()
display = term "display"

flex :: Term a => a
flex = term "flex"

someStyle :: Style
someStyle = Style $ do
    flex "1"     -- [1] :: StyleM ()
    display flex -- [2]
