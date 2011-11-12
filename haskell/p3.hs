isPrime 1 = False
isPrime 2 = True
isPrime arg = result
        where result = (length [x | x <- [2..(truncate $ sqrt (fromIntegral arg))], arg `mod` x == 0] == 0)
        
solveIt arg = maximum [y | y <- [x | x <- [1.. truncate $ sqrt (fromIntegral arg)]], arg `mod` y == 0, isPrime y]
