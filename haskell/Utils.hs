module Utils (primesTo) where

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
        