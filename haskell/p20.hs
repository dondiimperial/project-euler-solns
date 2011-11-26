import Data.Char
  
fact x = product [1..x]

main = do putStrLn . show $ sum $ map digitToInt $ show $ fact 100