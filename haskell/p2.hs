solveIt arg = sum (filter even (map fst (takeWhile (\a -> (fst a) < arg) (iterate (\(a,b) -> (b,a+b)) (0,1)))))