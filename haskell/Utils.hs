module Utils (primesTo, factorsOf, primeFactorsOf, primeFactorize) where

import Data.List

isPrime n =
  let 
    end = floor . sqrt . fromIntegral $ n
    primeTest [] = True
    primeTest (x:xs) =
      if n `mod` x == 0 then False
      else primeTest xs
  in primeTest $ [3,5..end]
  
primesTo m = filter isPrime $ 2:[3,5..m]

factorsOf m = filter (\x -> m `mod` x == 0)  [1..m]

primeFactorsOf m = filter (\x -> m `mod` x == 0) $ primesTo m

primeFactorize x = _primeFactorize x $ primeFactorsOf x
  where _primeFactorize 1 xs = []
        _primeFactorize x [] = [x]
        _primeFactorize x all@(y:ys)
          | x `mod` y == 0 = y:(_primeFactorize next nextYs)
          | otherwise = _primeFactorize x nextYs
          where next = truncate (fromIntegral x/fromIntegral y)
                nextYs = if next `mod` y == 0 then all else ys