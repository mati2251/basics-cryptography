minus :: (Ord a) => [a] -> [a] -> [a]
minus [] _ = []
minus xs [] = xs
minus (x : xs) (y : ys) = case compare x y of
  LT -> x : minus xs (y : ys)
  EQ -> minus xs ys
  GT -> minus (x : xs) ys

primesTo m = 2 : sieve [3, 5 .. m]
  where
    sieve (p : xs)
      | p * p > m = p : xs
      | otherwise = p : sieve (xs `minus` [p * p, p * p + 2 * p ..])

from = 10 ^ 10 - 10 ^ 5

to = 10 ^ 7

p = filter (>= from) $ primesTo to

main :: IO ()
main = print p

