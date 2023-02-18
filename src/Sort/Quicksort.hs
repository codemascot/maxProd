module Sort.Quicksort (qsort) where

-- Quicksort Starts
qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = lowest ++ [x] ++ highest
  where lowest  = qsort [a | a <- xs, a <= x]
        highest = qsort [a | a <- xs, a > x]
-- Quicksort Ends

