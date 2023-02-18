module Main where

import Text.Read

-- Quicksort Starts
qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = lowest ++ [x] ++ highest
  where lowest  = qsort [a | a <- xs, a <= x]
        highest = qsort [a | a <- xs, a > x]
-- Quicksort Ends

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

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

getProduct :: Num a => [a] -> a
getProduct xs = foldl (*) 1 xs

getFirstTwoAndLast :: [a] -> [a]
getFirstTwoAndLast xs = (take 2 xs) ++ (lastN 1 xs)

getLast3 :: [a] -> [a]
getLast3 xs = lastN 3 xs

highestProduct :: (Num a, Ord a) => a -> a -> a
highestProduct x y | x == y    = x
                   | x > y     = x
                   | otherwise = y
main :: IO ()
main = do
  putStrLn "Input your list here as [1, 10, 2, 5, 6, 3] : "
  xs  <- getLine
  let { ints :: Maybe [Int]
      ; ints = readMaybe xs
      }
  case ints of
    Nothing -> putStrLn "Invalid input!"
    Just ints -> do
      if (length ints) < 3 
        then putStrLn "Input list is too short to perform the operation." 
        else do
        let { sortedList :: [Int]
            -- ; sortedList = qsort ints
            ; sortedList = msort ints
            ; prodOfFirst2AndLast :: Int
            ; prodOfFirst2AndLast = getProduct $ getFirstTwoAndLast sortedList
            ; prodOfLast3 :: Int
            ; prodOfLast3 = getProduct $ getLast3 sortedList
            ; result :: Int
            ; result = highestProduct prodOfLast3 prodOfFirst2AndLast
            }
        print $ result
