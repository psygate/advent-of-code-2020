import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.List.Split
import Data.Char


--type Passport = (Maybe byr, Maybe iyr, Maybe eyr, Maybe hgt, Maybe hcl, Maybe ecl, Maybe pid, Maybe cid)
type Passport = (Maybe Int, Maybe Int, Maybe Int, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String)
emptyPassport = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

--    byr (Birth Year)
--    iyr (Issue Year)
--    eyr (Expiration Year)
--    hgt (Height)
--    hcl (Hair Color)
--    ecl (Eye Color)
--    pid (Passport ID)
--    cid (Country ID)

sanitizeLine :: String -> [String]
sanitizeLine line = splitOn ":" line

removeCM :: String -> String
removeCM value
  | map toLower (drop (length value - 2) value) == "cm" = reverse (drop 2 $ reverse value)
  | otherwise = error ("Cannot remove cm from " ++ (show value))

dropLast amount list = reverse $ drop amount (reverse list)

fillOutPassport :: [(String, String)] -> Passport -> Passport
fillOutPassport [] passport = passport
fillOutPassport ((valueType, value):xs) (byr, iyr, eyr, hgt, hcl, ecl, pid, cid)
  | valueType == "byr" = fillOutPassport xs (Just $ read value, iyr, eyr, hgt, hcl, ecl, pid, cid)
  | valueType == "iyr" = fillOutPassport xs (byr, Just $ read value, eyr, hgt, hcl, ecl, pid, cid)
  | valueType == "eyr" = fillOutPassport xs (byr, iyr, Just $ read value, hgt, hcl, ecl, pid, cid)
  | valueType == "hgt" = fillOutPassport xs (byr, iyr, eyr, Just value, hcl, ecl, pid, cid)
  | valueType == "hcl" = fillOutPassport xs (byr, iyr, eyr, hgt, Just value, ecl, pid, cid)
  | valueType == "ecl" = fillOutPassport xs (byr, iyr, eyr, hgt, hcl, Just value, pid, cid)
  | valueType == "pid" = fillOutPassport xs (byr, iyr, eyr, hgt, hcl, ecl, Just value, cid)
  | valueType == "cid" = fillOutPassport xs (byr, iyr, eyr, hgt, hcl, ecl, pid, Just value)
  | otherwise = error ((show valueType) ++ ":" ++ (show value))

toPassport :: [(String, String)] -> Passport
toPassport [] = emptyPassport
toPassport line = fillOutPassport line emptyPassport

listAsTuple2 :: [x] -> (x, x)
listAsTuple2 (x:y:[]) = (x, y)
--listAsTuple2 list
--  | length list == 2 = (head list, last list)
--  | otherwise = error "List with more or less than 2 values cannot be converted."


_extractRecords [] "" accumulator = accumulator
_extractRecords [] currentString accumulator = accumulator ++ [currentString]
_extractRecords (x:xs) currentString accumulator
  | x == ' ' || x == '\n' = _extractRecords xs "" (accumulator ++ [currentString])
  | otherwise = _extractRecords xs (currentString ++ [x]) accumulator

extractRecords [] = []
extractRecords list = _extractRecords list "" []


--    byr (Birth Year) - four digits; at least 1920 and at most 2002.
--    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
--    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
--    hgt (Height) - a number followed by either cm or in:
--        If cm, the number must be at least 150 and at most 193.
--        If in, the number must be at least 59 and at most 76.
--    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
--    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
--    pid (Passport ID) - a nine-digit number, including leading zeroes.
--    cid (Country ID) - ignored, missing or not.

eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidHeight hgt
  | suffix == "cm" = height >= 150 && height <= 193
  | suffix == "in" = height >= 59 && height <= 76
  | all isDigit hgt = False
  | otherwise = error ("Unknown height type: " ++ (hgt))
  where suffix = reverse (take 2 (reverse hgt))
        height = read (take (length hgt - 2) hgt)

isValidHairColor [] = False
isValidHairColor (x:xs) = (x == '#') && (all isHexDigit xs) && (length xs == 6)

isAValidPassport_1 (Just byr, Just iyr, Just eyr, Just hgt, Just hcl, Just ecl, Just pid, _) = True
isAValidPassport_1 _ = False

isAValidPassport_2 (Just byr, Just iyr, Just eyr, Just hgt, Just hcl, Just ecl, Just pid, _) =
  byr >= 1920 && byr <= 2002 &&
  iyr >= 2010 && iyr <= 2020 &&
  eyr >= 2020 && eyr <= 2030 &&
  isValidHeight hgt &&
  isValidHairColor hcl &&
  elem ecl eyeColors &&
  length pid == 9 &&
  all isDigit pid

isAValidPassport_2 _ = False

main = do
        input <- readFile "input.txt"
        let rawRecords = map extractRecords $ splitOn "\n\n" input
        let splitRecords = map (map (splitOn ":")) rawRecords
        let records = map (map listAsTuple2) splitRecords
        let passports = (map toPassport records)

        mapM print (zip [1..] $ zip records passports)
        print ("Valid passports 1: " ++ (show $ length $ filter isAValidPassport_1 passports))
        print ("Valid passports 2: " ++ (show $ length $ filter isAValidPassport_2 passports))

--        describe "isAValidPassport_2" $ do
--          it "is false for this passport" $ do
--            (isAValidPassport_2 toPassport([("eyr", "1972"), ("cid", "100")])) `shouldBe` (emptyPassport)

--        let records = (map (map (map listAsTuple2(splitOn ":"))) (map extractRecords $ splitOn "\n\n" input))
--        print records

--        mapM print (toPassport records)
--        print $ map toPassport $ splitOn "\n\n" input