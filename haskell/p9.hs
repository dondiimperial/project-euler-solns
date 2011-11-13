triplets = [[a,b,c] | a <- [1..1000], b <- [a+1..1000-a], c <- [1000 - a - b], a^2+b^2==c^2]

solveIt = product $ head triplets