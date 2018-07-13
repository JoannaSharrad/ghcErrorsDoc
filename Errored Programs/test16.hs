module Foo where

intersperse :: a -> [[a]] -> [a]

intersperse _ [] = []
intersperse _ [x] = x
intersperse s (x:y:xs) = x:s:y:intersperse s xs
