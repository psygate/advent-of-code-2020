data Terrain = Tree | Open | TreeStep | OpenStep
type MapRow = [Terrain]
type Map = [[Terrain]]

instance Show Terrain where
  show Tree = "#"
  show Open = "."
  show TreeStep = "X"
  show OpenStep = "O"

-- input -> current state -> return state
_parseData :: String -> [Terrain] -> Map -> Map
_parseData [] terrain state = state
_parseData (x:xs) terrain state
  | x == '.' = _parseData xs (terrain ++ [Open]) state
  | x == '#' = _parseData xs (terrain ++ [Tree]) state
  | x == '\n' = _parseData xs [] (state ++ [terrain])
  | otherwise = error ("Unknown character: " ++ [x])

parseData :: String -> Map
parseData "" = []
parseData str = _parseData str [] []

rotate_left _ [] = []
rotate_left amount list@(x:xs)
  | amount `mod` (length list) == 0 = list
  | amount < 0 = error "Use rotate left."
  | amount > 0 = rotate_left (amount - 1) (xs ++ [x])
  | amount == 0 = list

step_right :: Map -> Map
step_right terrain = map (rotate_left 1) terrain

step_down :: Map -> Map
step_down terrain@(x:xs) = xs

isATree Tree = True
isATree _ = False

isATreeAtIndex :: Int -> MapRow -> Bool
isATreeAtIndex idx row
  | idx < length row = isATree (row !! idx)
  | otherwise = error errormsg
  where errormsg = "Index out of bounds: " ++ (show idx) ++ "/" ++ (show (length row))

selectRow :: Int -> Map -> MapRow
selectRow idx [] = error "Cannot select index from empty list."
selectRow idx row@(x:xs)
  | idx == 0 = x
  | idx < length row = selectRow (idx - 1) xs
  | otherwise = error errormsg
  where errormsg = "Index out of bounds: " ++ (show idx) ++ "/" ++ (show (length row))

_walk :: (Int, Int) -> (Int, Int) -> Map -> Int -> Either String Int
_walk _ _ [] _ = error "Empty terrain"
_walk moveBasis@(moveX, moveY) (locX, locY) terrain treesEncountered
  | locY < length terrain = _walk moveBasis (moveX + locX, moveY + locY) terrain newTreeValue
  | otherwise = Right treesEncountered
  where newTreeValue = treesEncountered + (fromEnum (isATreeAtIndex normLocX currentRow))
        currentRow = selectRow locY terrain
        normLocX = mod locX (length currentRow)

walk movementTuple terrain = _walk movementTuple (0, 0) terrain 0

isLeftProjection (Left _) = True
isLeftProjection _ = False

isRightProjection (Right _) = True
isRightProjection _ = False

applyToRightProjection (Right r) op = Right (op r)
applyToRightProjection (Left r) op = Left (op r)

_repackLeft :: [Either x y] -> Either [x] [y] -> Either [x] [y]
_repackLeft [] x = x
_repackLeft ((Left x):xs) (Left list) = _repackLeft xs (Left (list ++ [x]))
_repackLeft ((Right x):xs) leftList = _repackLeft xs leftList

repackLeft :: [Either x y] -> Either [x] [y]
repackLeft list = _repackLeft list (Left [])

_repackRight :: [Either x y] -> Either [x] [y] -> Either [x] [y]
_repackRight [] x = x
_repackRight ((Right x):xs) (Right list) = _repackLeft xs (Right (list ++ [x]))

repackRight :: [Either x y] -> Either [x] [y]
repackRight list = _repackRight list (Right [])

_repackEither :: [Either x y] -> Either [x] [y] -> Either [x] [y] -> Either [x] [y]
_repackEither [] (Left []) rightAccum = rightAccum
_repackEither [] leftAccum rightAccum = leftAccum
_repackEither ((Left x):xs) (Left y) rightAccum = _repackEither xs (Left (x:y)) rightAccum
_repackEither ((Right x):xs) leftAccum (Right y) = _repackEither xs leftAccum (Right (x:y))

repackEither :: [Either x y] -> Either [x] [y]
repackEither [] = Right []
repackEither value = _repackEither value (Left []) (Right [])

main = do
        input <- readFile "input.txt"
        let forest = parseData input
        case (walk (3, 1) forest) of
          Right trees -> print ("Trees: " ++ (show trees))
          Left error -> print error

        let paths = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        let sums = map (\x -> walk x forest) paths
        let errors = repackEither sums

        case errors of
          Left errors -> print errors
          Right values -> print ("Values: " ++ (show (foldl (*) 1 values)))
