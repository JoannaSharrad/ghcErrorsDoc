main = interact $ show . maxsubseq . map read . words

maxsubseq :: (Ord a,Num a) => [a] -> (a,[a])
maxsubseq = snd . foldl f ((0,[]),(0,[])) where 
f ((h1,h2),sofar) x = (a,b) where
a = max (0,[]) (h1 + x ,h2 ++ [x]) 
b = max sofar a
