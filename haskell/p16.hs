import Data.Char
  
pow x 0 = 1
pow x 1 = x
pow x y = x * (pow x (y-1))

sumDigits x = sum $ map digitToInt $ show x

main = do putStrLn . show $ sumDigits $ pow 2 1000              