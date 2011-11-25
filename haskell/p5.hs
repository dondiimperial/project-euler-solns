import Utils
import Data.List

primeFactors n = foldr1 (union) $ map (group . primeFactorize) [1..n]

groupFactors [] ys = []
groupFactors xs [] = []                     
groupFactors xs (y:ys) = fst partitioned:groupFactors (snd partitioned) ys
  where partitioned = partition (\x -> head x == y) xs
                       
solveIt x = product $ map (product . last) $ filter (/=[]) $ groupFactors (primeFactors x) [1..x]

main = do putStrLn . show $ solveIt 20              

