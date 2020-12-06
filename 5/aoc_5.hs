import Data.List.Split
import Data.Char
import Data.List

type Row = Int
type Rows = [Row]

data SPlane = SFront | SBack | SLeft | SRight deriving Eq

instance Show SPlane where
  show SFront = "F"
  show SBack = "B"
  show SRight = "R"
  show SLeft = "L"

_toPlanes :: String -> [SPlane] -> [SPlane]
_toPlanes [] values = values
_toPlanes ('F':xs) list = _toPlanes xs (list ++ [SFront])
_toPlanes ('B':xs) list = _toPlanes xs (list ++ [SBack])
_toPlanes ('L':xs) list = _toPlanes xs (list ++ [SLeft])
_toPlanes ('R':xs) list = _toPlanes xs (list ++ [SRight])

toPlanes :: String -> [SPlane]
toPlanes str = _toPlanes str []

toTups :: [SPlane] -> ([SPlane], [SPlane])
toTups input = (filter (\x -> x == SFront || x == SBack) input, filter (\x -> x == SLeft || x == SRight) input)

verticalRows = [0..127] :: Rows
horizontalRows = [0..7] :: Rows

takeFrontHalf rows = take (div (length rows) 2) rows
takeBackHalf rows = drop (div (length rows) 2) rows

_applyVerticalPartitions :: [SPlane] -> Rows -> Row
_applyVerticalPartitions [] [x] = x
_applyVerticalPartitions (SFront:xs) rows = _applyVerticalPartitions xs (takeFrontHalf rows)
_applyVerticalPartitions (SBack:xs) rows = _applyVerticalPartitions xs (takeBackHalf rows)

applyVerticalPartitions :: [SPlane] -> Row
applyVerticalPartitions instructions = _applyVerticalPartitions instructions verticalRows

_applyHorizontalPartitions :: [SPlane] -> Rows -> Row
_applyHorizontalPartitions [] [x] = x
_applyHorizontalPartitions (SLeft:xs) rows = _applyHorizontalPartitions xs (takeFrontHalf rows)
_applyHorizontalPartitions (SRight:xs) rows = _applyHorizontalPartitions xs (takeBackHalf rows)

applyHorizontalPartitions :: [SPlane] -> Row
applyHorizontalPartitions instructions = _applyHorizontalPartitions instructions horizontalRows

seatId :: (Row, Row) -> Row
seatId (row, column) = row * 8 + column

_lmax :: [Row] -> Row -> Row
_lmax [] m = m
_lmax (x:xs) m
  | x > m = _lmax xs x
  | otherwise = _lmax xs m

lmax :: [Row] -> Row
lmax [x] = x
lmax (x:xs) = _lmax xs x

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge l@(x:xs) r@(y:ys)
  | x <= y = x : (merge xs r)
  | otherwise = y : (merge l ys)

findSeat (x:y:xs)
  | abs (x - y) == 2 = (x, y)
  | otherwise = findSeat (y:xs)

main = do
          input <- readFile "input.txt"
          let splitInput = map toPlanes (filter (\x -> x /= []) (splitOn "\n" input))
          let partitionedInputs = map toTups splitInput
          let verticalSeatNumbers = map applyVerticalPartitions (map fst partitionedInputs)
          let horizontalSeatNumbers = map applyHorizontalPartitions (map snd partitionedInputs)
          let seatNumbers = zip verticalSeatNumbers horizontalSeatNumbers
          let seatIds = sort $ map seatId seatNumbers
          let maxSeatId = last seatIds

          print ("Solution 1: " ++ (show maxSeatId))

          let (frontSeat, backSeat) = findSeat seatIds
          print ("Solution 2: " ++ (show (backSeat - 1)))

--          let twoDiffIds = filter (\(x,y) -> abs (y - x) == 1) (zip (drop 1 (sort seatIds)) (sort seatIds))
--          print maxSeatId
--
--          print $ filter (\(x,y) -> abs (x - y) == 2) twoDiffIds

--          print (sort seatIds)


main_ = do
          let input = ["BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"]
          let splitInput = map toPlanes input
          let partitionedInputs = map toTups splitInput
          let verticalSeatNumbers = map applyVerticalPartitions (map fst partitionedInputs)
          let horizontalSeatNumbers = map applyHorizontalPartitions (map snd partitionedInputs)
          let seatNumbers = zip verticalSeatNumbers horizontalSeatNumbers
          let seatIds = map seatId seatNumbers

          print seatIds
          print (lmax seatIds)
--
--    BFFFBBFRRR: row 70, column 7, seat ID 567.
--    FFFBBBFRRR: row 14, column 7, seat ID 119.
--    BBFFBBFRLL: row 102, column 4, seat ID 820.
