import Data.Char
  
pow :: (Integral a) => a -> a -> a
pow a 0 = 1       
pow a 1 = a
pow a b = a * (pow a (b-1))

digits :: (Integral a) => a -> [Int]          
digits = toIntArray . show
  where toIntArray = map (\x -> digitToInt x)

                
sumDigits = sum . digits

main = do putStrLn . show . sumDigits $ pow 2 1000