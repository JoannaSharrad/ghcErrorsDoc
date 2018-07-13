{-# LANGUAGE GADTs #-}

data My a where
  A :: Int  -> My Int
  B :: Char -> My Char


main :: IO ()
main = do
  let x = undefined :: My a

  case x of
    A v -> print v

  print x
