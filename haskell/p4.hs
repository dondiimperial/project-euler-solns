solveIt = maximum [x*y | x <- [xx | xx <- [100..999]],  y <- [yy | yy <- [100..999], yy /= x], show (x * y) == (reverse $ show (x * y))]