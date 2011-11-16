triangleNumbers = scanl1 (+) [1..]

countDivisors x = 1 + (length $ [xx | xx <- [1..floor ((fromIntegral x)/2)], x `mod` xx == 0])

findIt [] f = Nothing
findIt (x:xs) f
  | f x = Just x
  | otherwise = findIt xs f    

solve x = findIt triangleNumbers  (\xx -> (countDivisors xx) > x)

main = print $ solve 500                    

