{-# LANGUAGE PartialTypeSignatures #-}

import Data.List.Split(splitOn)
import Data.List
import Control.Applicative
import Control.Exception
import Debug.Trace
import Data.Array

data State = FLOOR | EMPTY | OCCUPIED | NIL deriving (Eq)

type FloorField = Array Int (Array Int State)

instance Show State where
  show FLOOR = [floorToChar]
  show EMPTY = [emptyToChar]
  show OCCUPIED = [occupiedToChar]
  show NIL = ['N']


newFloor :: State
newFloor = FLOOR


newEmpty :: State
newEmpty = EMPTY


newOccupied :: State
newOccupied = OCCUPIED


floorToChar :: Char
floorToChar = '.'


emptyToChar :: Char
emptyToChar = 'L'


occupiedToChar :: Char
occupiedToChar = '#'


stateFromChar :: Char -> State
stateFromChar '.' = FLOOR
stateFromChar '#' = OCCUPIED
stateFromChar 'L' = EMPTY

charToState = stateFromChar


isOccupied :: State -> Bool
isOccupied OCCUPIED = True
isOccupied _ = False


isEmpty :: State -> Bool
isEmpty EMPTY = True
isEmpty _ = False


isFloor :: State -> Bool
isFloor FLOOR = True
isFloor _ = False


inputAsList :: String -> [[State]]
inputAsList str = map (map charToState) (lines str)


prettyString :: [[State]] -> _
prettyString state = foldl (++) "" stateLinesWithEndings
  where statesAsStrings = map (map show) state
        stateLines = map (foldl (++) "") statesAsStrings
        stateLinesWithEndings = map (\x -> x ++ "\n") stateLines


fill :: b -> [a] -> [b]
fill a [] = []
fill a (x:xs) = a:(fill a xs)


count :: (Eq a) => a -> [a] -> Int
count value list = length $ filter ((==) value) list


nextCellState :: State -> [State] -> State
nextCellState EMPTY stateList
  | count OCCUPIED stateList == 0 = OCCUPIED
  | otherwise = EMPTY

nextCellState OCCUPIED stateList
  | count OCCUPIED stateList >= 4 = EMPTY
  | otherwise = OCCUPIED

nextCellState state _ = state


iterateKernel :: [State] -> [State] -> [State] -> [State]
iterateKernel (l1:l2:l3:[]) (c1:c2:c3:[]) (r1:r2:r3:[]) = [nextCellState c2 [l1, l2, l3, c1, c3, r1, r2, r3]]
iterateKernel (l1:l2:l3:ls) (c1:c2:c3:cs) (r1:r2:r3:rs) = headState:tailStates
  where headState = nextCellState c2 [l1, l2, l3, c1, c3, r1, r2, r3]
        tailStates = iterateKernel (l2:l3:ls) (c2:c3:cs) (r2:r3:rs)


iterateKernelOverRow :: [State] -> [State] -> [State] -> [State]
iterateKernelOverRow left current right = iterateKernel paddedLeft paddedCurrent paddedRight
  where paddedLeft = NIL:left ++ [NIL]
        paddedCurrent = NIL:current ++ [NIL]
        paddedRight = NIL:right ++ [NIL]


threeRowMap :: [[a]] -> [([a], [a], [a])]
threeRowMap (l:c:r:[]) = [(l, c, r)]
threeRowMap (l:c:r:xs) = (l, c, r):(threeRowMap (c:r:xs))


iterateKernelOverSquare :: [[State]] -> [[State]]
iterateKernelOverSquare state = map (\(x, y, z) -> iterateKernelOverRow x y z) rowGroups
  where psize = length $ head state
        size = assert (all (\x -> length x == psize) state) psize
        padding = replicate size NIL :: [State]
        paddedState = padding:state ++ [padding] :: [[State]]
        rowGroups = threeRowMap paddedState :: [([State], [State], [State])]


eqassert :: (Show a) => (Eq a) => (Show b) => a -> a -> b -> String
eqassert a b testID
  | a == b = "{" ++ (show testID) ++ "} [+] " ++ (show a)
  | otherwise = error ("{" ++ (show testID) ++ "} >> " ++ (show a) ++ " /= " ++ (show b))


test :: String -> IO ()
test inputname = do
        rawinput <- readFile inputname
        let input = inputAsList rawinput

        let seperator = putStrLn $ replicate 40 '-'
        let emptyStateList = [] :: [State]
        let twice = replicate 2
        let thrice = replicate 3

        putStrLn $ eqassert (iterateKernel (thrice FLOOR) (thrice FLOOR) (thrice FLOOR)) [FLOOR] "1.0.1"
        putStrLn $ eqassert (iterateKernel (thrice EMPTY) (thrice EMPTY) (thrice EMPTY)) [OCCUPIED] "1.0.2"
        putStrLn $ eqassert (iterateKernel (thrice OCCUPIED) (thrice OCCUPIED) (thrice OCCUPIED)) [EMPTY] "1.0.3"
        putStrLn $ eqassert (iterateKernel (thrice FLOOR) [FLOOR, EMPTY, FLOOR] (thrice FLOOR)) [OCCUPIED] "1.0.4"
        putStrLn $ eqassert (iterateKernel (thrice OCCUPIED) [OCCUPIED, EMPTY, OCCUPIED] (thrice OCCUPIED)) [EMPTY] "1.0.4"

        let s1 = map charToState "#.##.##.##"
        let s2 = map charToState "#######.##"
        let es1 = map charToState "#.LL.L#.##"

        putStrLn $ eqassert (iterateKernelOverRow (fill NIL s1) s1 s2) es1 "2.0.1"

        rawinput1 <- readFile "test_input.txt"
        rawoutput <- readFile "test_output.txt"

        let input1 = inputAsList rawinput1
        let outputs = map inputAsList (splitOn "\n\n" rawoutput)

        let inputIterator = drop 1 $ iterate iterateKernelOverSquare input1

        mapM_ (\(x, y) -> putStrLn x >> putStrLn y >> seperator) $ map (\(x, y) -> (prettyString x, prettyString y)) $ zip inputIterator outputs

        let valuePairs = map (\(i, (x, y)) -> (i, prettyString x, prettyString y)) $ zip [0..] $ zip inputIterator outputs
        mapM_ putStrLn $ map (\(i, x, y) -> eqassert x y ("3.0." ++ (show i))) valuePairs

        return ()

part1 :: String -> IO ()
part1 inputname = do
        rawinput <- readFile inputname
        let input = inputAsList rawinput

        let left = iterate iterateKernelOverSquare input
        let right = drop 1 $ iterate iterateKernelOverSquare input

        let seperator = replicate 40 '-'
        let stabilize = fst . head . dropWhile (\(x, y) -> x /= y)

        putStrLn $ prettyString $ stabilize $ zip left right
        putStrLn $ show (count OCCUPIED ([x | y <- stabilize $ zip left right, x <- y]))

main :: IO ()
main = do
        test "test_input.txt"
        part1 "input.txt"

