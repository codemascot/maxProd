module Sort.MergeSort (msort) where

-- Merge Sort Starts
-- Divide a list into two halves
divide :: (Ord a) => [a] -> ([a], [a])
divide []     = ([], [])
divide (x:xs) = (x:ys, zs)
  where
    (zs, ys) = divide xs

-- Merge two sorted lists
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] []         = []
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys) | x <= y    = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

-- Sort the list using the merge sort algorithm
msort :: (Ord a) => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = (merge . msort) x $ msort y
  where
    (x, y) = divide xs
-- Merge Sort Ends

