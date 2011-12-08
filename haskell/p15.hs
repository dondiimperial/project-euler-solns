solveIt x = last . last . compute $ matrix
  where compute = _compute 1 1
        first = 0:(take x $ repeat 1)
        matrix = first:(take x . repeat $ 1:(take x $ repeat 0))
        _compute xx yy xs
          | yy > x = xs
          | xx > x = _compute 1 (yy+1) xs
          | otherwise = _compute (xx+1) yy $ (fst splitmatrix) ++ [subst] ++ (tail . snd $ splitmatrix)
          where prevline = xs !! (yy-1)
                curline = xs !! yy
                splitline = splitAt xx curline
                subst = (fst splitline) ++ [(curline !! xx) + (curline !! (xx-1)) + (prevline !! xx)] ++  (tail . snd $ splitline)
                splitmatrix = splitAt yy xs
        
main = do putStrLn . show $ solveIt 20        
        
