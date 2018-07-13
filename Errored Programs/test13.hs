{-# LANGUAGE ImplicitParams #-}

main = print (f z)

f g =
  let 
    ?x = 42
    ?y = 5
  in
    g

z :: (?x :: Int) => Int
z = ?x
