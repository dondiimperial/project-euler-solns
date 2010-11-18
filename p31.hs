denom = [1,2,5,10,20,50,100,200,400]

counts = reverse [0 : reverse (takeWhile (< 200) (scanl1 (+) (repeat x))) | x <- denom]


result [] total = 0
result _ 0 = 1
result (x:xs) total = result xs (total-x)

ret = [[a,b,c,d,e,f,g,h] | a <- (counts !! 0), b <- (counts !! 1), c <- (counts !! 2), d <- (counts !! 3), e <- (counts !! 4), f <- (counts !! 5), g <- (counts !! 6), h <- (counts !! 7), a + b + c + d + e + f + g + h == 200]

rret = result [1,2,5,10,20,50,100,200] 200