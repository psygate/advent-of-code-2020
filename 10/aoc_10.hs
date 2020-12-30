import Data.List.Split(splitOn)
import Data.List
import Control.Applicative
import Control.Exception
import Debug.Trace



stringToNumberList :: String -> [Int]
stringToNumberList d = sort $ map read (lines d)


isConnectable :: Int -> Int -> Bool
isConnectable a b = diff >= 1 && diff <= 3 where diff = abs (a - b)


isValidChain :: [Int] -> Bool
isValidChain [] = True
isValidChain chain = all (\(x, y) -> isConnectable x y) $ zip chain (drop 1 chain)


diffs :: [Int] -> [Int]
diffs (x:y:[]) = [diff] where diff = abs (x - y)
diffs (x:y:xs) = diff:(diffs (y:xs)) where diff = abs (x - y)


finalOutput :: [Int] -> Int
finalOutput chain = assert (threes + ones == length diffs) (threes * ones)
  where diffs = map (\(x, y) -> abs (x - y)) $ zip chain (drop 1 chain)
        threes = length $ filter ((==) 3) diffs
        ones = length $ filter ((==) 1) diffs

test4 :: IO ()
test4 = do
          print $ combinations [1, 2]
          print $ combinations [1, 2, 3]


part1 :: String -> IO ()
part1 input = do
          rawinput <- readFile input
          let input = stringToNumberList rawinput
          let inputRating = 0
          let outputRating = 3 + maximum input
          let fullChain = [inputRating] ++ input ++ [outputRating]

          print $ assert (isValidChain fullChain) "[+] " ++ (show $ finalOutput fullChain)

          return ()


part2 :: String -> IO ()
part2 input = do
          rawinput <- readFile input
          let input = stringToNumberList rawinput
          let inputRating = 0
          let outputRating = 3 + maximum input
--          let validPermutations = filter isValidChain (permutations input)
--          let arrangements = map (\x -> [inputRating] ++ (sort x) ++ [outputRating]) validPermutations
--          let validArrangements = filter isValidChain arrangements
--
--          print $ length validArrangements

          return ()


test1 :: String -> IO ()
test1 input = do
          rawinput <- readFile input
          let input = stringToNumberList rawinput
          let inputRating = 0
          let outputRating = 3 + maximum input
          let fullChain = [inputRating] ++ input ++ [outputRating]

          print $ assert (isValidChain fullChain) "[+] " ++ (show $ finalOutput fullChain)
          return ()



test2 :: String -> IO ()
test2 input = do
          rawinput <- readFile input
          let input = stringToNumberList rawinput
          let inputRating = 0
          let outputRating = 3 + maximum input
          let fullChain = [inputRating] ++ input ++ [outputRating]

          print $ assert (isValidChain fullChain) "[+] " ++ (show $ finalOutput fullChain)

          return ()


main :: IO ()
main = do
          test1 "test_input.txt"
          test2 "test_input_2.txt"
          part1 "input.txt"
          part2 "input.txt"

          test4

          return ()