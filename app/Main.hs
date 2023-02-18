module Main where

import Text.Read
import Sort.Quicksort (qsort)
import Sort.MergeSort (msort)

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
