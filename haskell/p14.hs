import Data.List
  
chain 1 = 1
chain n
  | even n = 1 + (chain $ (floor $ (fromIntegral n) / 2))
  | otherwise = 1 + (chain $ (3*n) + 1)

eval acc@(a,b) x
  | a > xx = acc
  | otherwise = (x, xx)
  where xx = chain x

solution n = foldl' eval (0,0) [1..n]

solveIt = solution 999999

main = do putStrLn . show $ solveIt          