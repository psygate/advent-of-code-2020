import Data.List.Split
import Control.Applicative
import qualified Data.Set as Sets

nonEmpty :: [x] -> Bool
nonEmpty [] = False
nonEmpty _ = True

maybeIntersect :: Ord x => Maybe (Sets.Set x) -> Maybe (Sets.Set x) -> Maybe (Sets.Set x)
maybeIntersect Nothing a = a
maybeIntersect a Nothing = a
maybeIntersect a Nothing = a
maybeIntersect (Just a) (Just b) = Just $ Sets.intersection a b

unpackMaybe :: Maybe x -> x
unpackMaybe (Just a) = a
unpackMaybe Nothing = error "Nothing can't be unpacked."

main :: IO ()
main = do
      input <- readFile "input.txt"
      let groups = map (filter nonEmpty) (map (splitOn "\n") (splitOn "\n\n" input))
      let setGroups = map (map Sets.fromList) groups
      let combinedGroups = map Sets.unions setGroups

      print ("Solution 1: " ++ (show $ sum (map length combinedGroups)))

      let justGroups = map (map Just) setGroups
      let intersectedGroups = map (foldl maybeIntersect Nothing) justGroups

      print ("Solution 2: " ++ (show $ length $ foldl (++) "" (map Sets.elems (map unpackMaybe intersectedGroups))))
