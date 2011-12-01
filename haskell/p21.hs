import Utils
import Data.List

d = sum . init . factorsOf

ds limit = sort . filter (\x -> fst x /= snd x) . filter (\x -> (d $ fst x) == (snd x)) $ map (\x -> (d x, x)) [2..limit-1]

main = putStrLn . show . sum . map fst $ ds 10000           