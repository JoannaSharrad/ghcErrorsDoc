--https://stackoverflow.com/questions/23702891/overlapping-instances-for-between-double-and-integral-types
class StatType a where
  toDouble :: a -> Double
instance StatType Double where
  toDouble = id
instance Integral a => StatType a where
  toDouble = fromIntegral

avg :: StatType a => [a] -> Double
avg = undefined
