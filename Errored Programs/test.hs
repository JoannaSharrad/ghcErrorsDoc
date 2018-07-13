module Foo where
  addThree = \x -> x + 3 :: Int
  y = addThree $ Just 5
