type PasswordRecord = (Int, Int, Char, String)

--show (minOcc, maxOcc, chr, pw) = "(" ++ (show minOcc) ++ ", " ++ (show maxOcc) ++ ", " ++ ([chr]) ++ ", " ++ pw ++ ")"

-- Find a character in a string, zero indexed. (findChar 't' "tea" = 0, findChar 'a' "tea" = 2)
-- -1 if the character doesn't exist in the string.
indexOf :: Eq x => x -> [x] -> Int
indexOf x [] = -1
indexOf x y
  | (head y) == x = 0
  | nextIndex == -1 = -1
  | otherwise = 1 + nextIndex
  where
    nextIndex = indexOf x (drop 1 y)

count c l = length $  filter (\x -> x == c) l

_split :: Eq x => x -> [x] -> [[x]] -> [[x]]
_split x [] y = y
_split x remaining outputList
  | nextIndex == -1 = outputList ++ [remaining]
  | nextIndex == 0 = _split x (drop 1 remaining) outputList
  | otherwise = _split x (drop nextIndex remaining) (outputList ++ [(take nextIndex remaining)])
  where
    nextIndex = indexOf x remaining

split :: Eq x => x -> [x] -> [[x]]
split x [] = []
split x y = _split x y []


lineToRecord :: String -> PasswordRecord
lineToRecord str = (minOccurances, maxOccurances, restrictedChar, password)
  where
    splitString = split ' ' str
    occurances = split '-' (head splitString)
    minOccurances = read (head occurances)
    maxOccurances = read (last occurances)
    restrictedChar = head $ head $ tail splitString
    password = last splitString

isValidPassword :: PasswordRecord -> Bool
isValidPassword (minOcc, maxOcc, chr, pw) = chrCount >= minOcc && chrCount <= maxOcc
  where chrCount = count chr pw

passwordHasCharAtIndex pw index chr
  | index < length pw && index >= 0 = (pw !! index) == chr
  | otherwise = False

isValidPasswordNewPolicy :: PasswordRecord -> Bool
isValidPasswordNewPolicy (indexOne, indexTwo, chr, pw) = (sum $ map fromEnum newPw) == 1
  where newPw = [passwordHasCharAtIndex pw (indexOne - 1) chr, passwordHasCharAtIndex pw (indexTwo - 1) chr]


main :: IO ()
main = do
        x <- readFile "input"
        let lines = split '\n' x
        let records = map lineToRecord lines
        putStr $ "Valid Pws: " ++ (show (length $ filter isValidPassword records)) ++ "\n"
        putStr $ "New Policy Valid Pws: " ++ (show (length $ filter isValidPasswordNewPolicy records)) ++ "\n"