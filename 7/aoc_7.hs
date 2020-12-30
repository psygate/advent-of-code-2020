{-# LANGUAGE PartialTypeSignatures #-}

import Data.List.Split hiding (startsWith, endsWith)
import Data.List hiding (union)
import Data.String
import Control.Applicative
import Data.Char
import qualified Data.Set as Sets
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Control.Exception
import Debug.Trace

type BagColor = String
type BagT = (BagColor, [(Int, BagColor)])
type BagColorSet = Sets.Set String


startsWith :: String -> String -> Bool
startsWith [] [] = False
startsWith [] a = False
startsWith a [] = False
startsWith (x:[]) (y:[]) = x == y
startsWith (x:[]) (y:ys) = x == y
startsWith (x:xs) (y:[]) = x == y
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys


endsWith :: String -> String -> Bool
endsWith a b = startsWith (reverse a) (reverse b)


newBagColor :: String -> BagColor
newBagColor value = value


dropBagSuffix :: String -> String
dropBagSuffix str
  | endsWith str " bags" = take (length str - 5) str
  | endsWith str " bag" = take (length str - 4) str
  | endsWith str "bags" = take (length str - 4) str
  | endsWith str "bag" = take (length str - 3) str
  | otherwise = error ("Illegal bag suffix value: " ++ str)


extractBags :: String -> _ -- (BagColor, [(Int, BagColor)])
extractBags value
  | tail == "no other bags" = (bagColor, [])
  | otherwise = (bagColor, containedBags)
  where noDotValue = assert (last value == '.') (init value)
        (bagColor:tail:[]) = splitOn " bags contain " noDotValue
        containedBagStrings = splitOn ", " tail
        trimBagSpec = dropBagSuffix . dropWhile isSpace . dropWhile isDigit
        containedBags = map (\x -> (read $ takeWhile isDigit x, trimBagSpec x)) containedBagStrings :: [(Int, BagColor)]


containsBagList :: BagColor -> [(Int, BagColor)] -> Bool
containsBagList color list = elem color (map (\(x,y) -> y) $ filter (\(x,y) -> x > 0) list)


containsBag :: BagColor -> BagT -> Bool
containsBag color (_, list) = containsBagList color list


fix :: (Eq a) => (a -> a) -> a -> a
fix fun value
  | value == nextValue = nextValue
  | otherwise = fix fun nextValue
  where nextValue = fun value


unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:[]) = [x]
unique (x:xs)
  | elem x xs = unique xs
  | otherwise = x:unique xs


flatten :: [[a]] -> [a]
flatten [] = []
flatten listOfLists = [element | x <- listOfLists, element <- x]


data Bag = Bag BagColor [Bag] deriving (Eq, Show)


data BinaryTree a = Leaf | BNode a (BinaryTree a) (BinaryTree a) -- deriving (Show)


newBinaryTree :: (Ord a) => a -> (BinaryTree a)
newBinaryTree value = BNode value Leaf Leaf


emptyBinaryTree :: BinaryTree a
emptyBinaryTree = Leaf


binaryTreeToList :: BinaryTree a -> [a]
binaryTreeToList Leaf = []
binaryTreeToList (BNode value leftChild rightChild) = (binaryTreeToList rightChild) ++ [value] ++ (binaryTreeToList leftChild)


binaryTreeFromList' :: (Ord a) => [a] -> BinaryTree a -> BinaryTree a
binaryTreeFromList' [] tree = tree
binaryTreeFromList' (x:xs) tree = binaryTreeFromList' xs (insertBinaryTree tree x)


binaryTreeFromList :: (Ord a) => [a] -> BinaryTree a
binaryTreeFromList [] = emptyBinaryTree
binaryTreeFromList list = binaryTreeFromList' list emptyBinaryTree


insertBinaryTree :: (Ord a) => (BinaryTree a) -> a -> (BinaryTree a)
insertBinaryTree Leaf value = BNode value Leaf Leaf

insertBinaryTree (BNode lvalue Leaf Leaf) rvalue
  | lvalue <= rvalue = BNode lvalue newBNode empty
  | otherwise = BNode lvalue empty newBNode
  where empty = emptyBinaryTree
        newBNode = newBinaryTree rvalue

insertBinaryTree (BNode lvalue leftChild Leaf) rvalue
  | lvalue <= rvalue = BNode lvalue newLeftChild empty
  | otherwise = BNode lvalue leftChild newBNode
  where empty = emptyBinaryTree
        newBNode = newBinaryTree rvalue
        newLeftChild = insertBinaryTree leftChild rvalue

insertBinaryTree (BNode lvalue Leaf rightChild) rvalue
  | lvalue <= rvalue = BNode lvalue newBNode rightChild
  | otherwise = BNode lvalue empty newRightChild
  where empty = emptyBinaryTree
        newBNode = newBinaryTree rvalue
        newRightChild = insertBinaryTree rightChild rvalue

insertBinaryTree (BNode lvalue leftChild rightChild) rvalue
  | lvalue <= rvalue = (BNode lvalue newLeftChild rightChild)
  | otherwise = (BNode lvalue  leftChild newRightChild)
  where newLeftChild = insertBinaryTree leftChild rvalue
        newRightChild = insertBinaryTree rightChild rvalue


instance (Show a) => Show (BinaryTree a) where
  show Leaf = "{}"
  show (BNode value Leaf Leaf) = "{" ++ (show value) ++ "}"
  show (BNode value rightChild leftChild) = "{" ++ (show value) ++ ", " ++ (show leftChild) ++ ", " ++ (show rightChild) ++ "}"


treeHasChild :: [BagT] -> String -> BagT -> Bool
--treeHasChild bagList findcolor (currentBagColor, []) = currentBagColor == findcolor
treeHasChild bagList findcolor (currentBagColor, currentBagChildren)
  | currentBagColor == findcolor = True
  | currentBagChildren == [] = trace ("Current bag empty " ++ currentBagColor) False
  | otherwise = any (treeHasChild' bagList findcolor) currentBagChildrenColors'
  where currentBagChildrenColors = map (\(_, ccolor) -> ccolor) currentBagChildren :: [String]
        currentBagChildrenColors' = trace ("Children colors of " ++ currentBagColor ++ ": " ++ (show currentBagChildren)) currentBagChildrenColors

treeHasChild' :: [BagT] -> String -> String -> Bool
treeHasChild' bags findcolor currentBagColor = treeHasChild bags findcolor selectedBag
  where (selectedBag:[]) = filter (\(x,y) -> x == currentBagColor) bags


hasChild :: [BagT] -> String -> String -> Int
hasChild [] _ _ = 0
hasChild bags findcolor currentBagColor
  | length currentbaglist /= 1 = error ("Current bag " ++ (show currentBagColor) ++ " not in bag list.")
  | children == [] = 0
  | elem findcolor childrenColors = matchingChildAmount
  | otherwise = sum childSearches
  where currentbaglist = filter (\(bagcolor, _) -> bagcolor == currentBagColor) bags
        (currentBag:[]) = currentbaglist
        (_, children) = currentBag
        childrenColors = map (\(_, childBagColor) -> childBagColor) children
        ((matchingChildAmount, _):[]) = filter (\(_, childColor) -> findcolor == childColor) children
        childSearches = map (\(amount, childColor) -> amount * hasChild bags findcolor childColor) children


countChildren' :: [BagT] -> String -> Int
countChildren' dict bagcolor
  | children == [] = 1
  | otherwise = 1 + sum amounts
  where ((_, children):[]) = filter (\(color, _) -> color == bagcolor) dict
        amounts = map (\(amount, childbagcolor) -> amount * (countChildren' dict childbagcolor)) children


countChildren :: [BagT] -> String -> Int
countChildren dict bagcolor = (countChildren' dict bagcolor) - 1

tests :: String -> IO ()
tests inputname = do
        rawinput <- readFile inputname
        let splitLines = lines rawinput
        let bagTuples = map extractBags splitLines :: [BagT]
        let bagColors = map (\(color, _) -> color) bagTuples
        let flatBagTuples = map (\(color, list) -> (color, [t | (i, c) <- list, t <- replicate i c])) bagTuples
        let bagTypes = unique $ (map (\(c, _) -> c) flatBagTuples) ++ (flatten $ map (\(_, c) -> c) flatBagTuples)

        print $ assert (length bagTuples == length bagTypes) "Success unique"

        let testDict = [  ("blue", [(1, "green"), (1, "red"), (2, "yellow")]),
                          ("red", []),
                          ("green", [(1, "purple"), (1, "pink")]),
                          ("pink", [(1, "orange")]),
                          ("purple", []),
                          ("orange", []),
                          ("yellow", [(2, "brown"), (4, "beige")]),
                          ("brown", []),
                          ("beige", [])
                        ]

        let hasChild' d a b = hasChild d b a

        print $ assert (hasChild' testDict "blue" "green" == 1) "Success 1"
        print $ assert (hasChild' testDict "blue" "red" == 1) "Success 2"
        print $ assert (hasChild' testDict "blue" "orange" == 1) "Success 3"
        print $ assert (hasChild' testDict "blue" "pink" == 1) "Success 4"
        print $ assert (hasChild' testDict "blue" "yellow" == 2) "Success 5"
        print $ assert (hasChild' testDict "blue" "brown" == 4) "Success 6"
        print $ assert (hasChild' testDict "yellow" "brown" == 2) "Success 7"
        print $ assert (hasChild' testDict "blue" "brown" == 4) "Success 8"
        print $ assert (hasChild' testDict "blue" "beige" == 8) "Success 9"


part1 :: String -> IO ()
part1 inputname = do
        rawinput <- readFile inputname
        let splitLines = lines rawinput
        let bagTuples = map extractBags splitLines :: [BagT]
        let bagColors = map (\(color, _) -> color) bagTuples

        let contains = map (hasChild bagTuples "shiny gold") bagColors
        print $ "Part1: " ++ (show (length $ filter ((/=) 0) contains))


part2 :: String -> IO ()
part2 inputname = do
        rawinput <- readFile inputname
        let splitLines = lines rawinput
        let bagTuples = map extractBags splitLines :: [BagT]

        print $ countChildren bagTuples "shiny gold"


main :: IO ()
main = do
--        part1 "input.txt"
        part2 "input.txt"

        return ()
