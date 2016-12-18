module NSpell
( empty
, observe
, correction
) where

import Data.List
import Data.Ord
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Text as T

type WordCount = Map.Map String Int
type StringSet = Set.Set String

empty :: WordCount
empty = Map.empty

observe :: WordCount -> String -> WordCount
observe wc w = Map.insert w ((Map.findWithDefault 0 w wc) + 1) wc

p :: WordCount -> String -> Double
p wc w = count / total
    where count = fromIntegral $ Map.findWithDefault 0 w wc
          total = fromIntegral $ Map.foldr (+) 0 wc

correction :: WordCount -> String -> String
correction wc w = maximumBy (comparing (p wc)) (candidates wc w)

candidates :: WordCount -> String -> StringSet
candidates wc w = head $ filter (not . Set.null) [w',e1,e2,Set.singleton w]
    where known cs = Set.intersection cs $ Map.keysSet wc
          w' = known $ Set.fromList [w]
          e1 = known $ edit w
          e2 = known $ Set.unions [ edit e | e <- Set.toList $ edit w ]

edit :: String -> StringSet
edit w =
    Set.unions $ map Set.fromList [deletes, transposes, replaces, inserts]
    where splits = [(take i w, drop i w) | i <- [0..length w]]
          deletes = [l ++ rs | (l,(r1:rs)) <- splits]
          transposes = [l ++ (r2:r1:rs) | (l,(r1:r2:rs)) <- splits]
          replaces = [l ++ c : rs | (l,(r1:rs)) <- splits, c <- ['a'..'z']]
          inserts = [l ++ c : r | (l,r) <- splits, c <- ['a'..'z']]
