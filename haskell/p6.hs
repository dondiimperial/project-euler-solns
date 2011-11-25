square x = x * x
sumOfSquares = sum [square x | x <- [1..100]]
squareOfSums = square $ sum [x | x <- [1..100]]
main = do putStrLn . show $ squareOfSums - sumOfSquares