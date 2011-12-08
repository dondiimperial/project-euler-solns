ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty"]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

nameIt x
  | x < 21 = ones !! x
  | x < 100 = tens !! (floor $ (fromIntegral x)/10) ++ (nameIt $ x `mod` 10)
  | x < 1000 = ones !! (floor $ (fromIntegral x)/100) ++ "hundred" ++ (if x `mod` 100 == 0 then "" else "and") ++ (nameIt $ x `mod` 100)
  | x < 10000 = ones !! (floor $ (fromIntegral x)/1000) ++ "thousand" ++  (if x `mod` 1000 == 0 then "" else "and") ++ (nameIt $ x `mod` 1000)
  | otherwise = "UNSUPPORTED"

solveIt = sum . take 1000 $ map length $ map nameIt [1..]

main = do putStrLn . show $ solveIt          