primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

nPrime x = primes !! (x - 1)

main = do putStrLn . show $ nPrime 10001