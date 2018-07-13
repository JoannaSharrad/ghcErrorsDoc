module Foo where
test = curry (.)

  addThree = \x -> x + 3 :: Int
  y = addThree $ Just 5
