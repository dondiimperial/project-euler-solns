qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
      let sSorted = qsort [a | a <- xs, a <= x]
          bSorted = qsort [a | a <- xs, a > x]

      in sSorted ++ [x] ++ bSorted

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

noneOf [] [] = False
noneOf _ [] = False
noneOf [] _ = False
noneOf (x:xs) (y:ys)
       | x == y = True
       | noneOf (x:xs) ys = True
       | noneOf xs (y:ys) = True
       | otherwise = False
       

fPrimes = [x | x <- takeWhile(<10000) primes, x > 1487, noneOf "1379" $ show x ]

ones x = mod x  10
tens x = ones $ quot x 10
hunds x = ones $ quot x 100
ths x = ones $ quot x 1000

toA x = qsort $ show x

triples = [[x,y,z] | x <- fPrimes, y <- [yy | yy <- fPrimes, yy > x], z <- [zz | zz <- fPrimes, zz > y], y - x == z - y, toA x == toA y, toA y == toA z]

