import System.IO
import Data.List  
import Data.Char
  
computeScore x = sum $ map (\xx -> (ord xx) - 64) x

computeScores xs =  [(computeScore y) * z | x <- zip parsedNames [1..], y <- [fst x], z <- [snd x]]
  where parsedNames = sort $ filter (all (isAlpha)) $ groupBy (\x y -> isAlpha x == isAlpha y) xs

computeScoreTotal = sum . computeScores
                       
main = do
  contents <- readFile "names.txt"
  putStrLn . show . computeScoreTotal $ contents