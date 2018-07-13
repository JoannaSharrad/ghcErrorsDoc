module Main where
main :: IO()
main = interact (unlines.strout.calc.extinps.words)

--calculates factorial
factorial :: Integral a=> a->a
factorial n = product [1..n]

--Extracts numbers from the input
extinps ::(Read b)=>[String]->[b]
extinps x=map read x

--Calculates the factorial
calc :: (Integral b) => [b] -> [b]
calc x= map factorial x

--Converts the result to a string
strout::(Show b)=>[b]->[[Char]]
strout x=[show a|a<-x]
