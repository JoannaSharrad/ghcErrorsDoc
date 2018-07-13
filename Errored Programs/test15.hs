{-# LANGUAGE GeneralizedNewtypeDeriving #-}

 class NameOf a where
   nameOf :: proxy a -> String

 instance NameOf Int where
   nameOf _ = "Int"

 newtype MyInt = MyInt Int
   deriving (NameOf) 
