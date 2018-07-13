newtype Poly a = P [a]

apply :: Num a => Poly a -> Num a -> Num a
apply (P p) x = if (i > 0) then (x * ((head p) ^ i)) + (apply (P (tail p)) x) else p
    where  i = length p
