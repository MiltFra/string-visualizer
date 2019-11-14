module Parser where

import           Data.List
import Data.Sort

newtype Map a =
  M [a]
  deriving (Show)

data Trans =
  T (Map Char) [(Int, Int)]
  deriving (Show)

findInMap :: Eq a => Map a -> a -> Int
findInMap (M l) x = f $ findIndex (x ==) l
  where
    f Nothing  = error "Element not in map"
    f (Just a) = a

len :: Map a -> Int
len (M l) = length l


symbols :: Trans -> [Char]
symbols (T (M l) _) = l

transSeq :: Trans -> [(Int, Int)]
transSeq (T _ s) = s

mkTrans :: String -> Trans
mkTrans str = T symbs (mapSeq symbs str)
  where
    symbs = mkMap str

mapSeq :: Map Char -> String -> [(Int, Int)]
mapSeq m str = zipIntOccur (map (findInMap m) str)

zipIntOccur :: [Int] -> [(Int, Int)]
zipIntOccur xs = f (mkMap xs) (replicate (length $ nub xs) 0) xs
  where
    f :: Map Int -> [Int] -> [Int] -> [(Int, Int)]
    f _ _ [] = []
    f m o (x:xs) = (x, o !! i) : f m (inc o i) xs
      where
        i = findInMap m x

inc :: [Int] -> Int -> [Int]
inc xs i = take i xs ++ incHead (drop i xs)
  where
    incHead xs = (head xs) + 1 : tail xs

mkMap :: Ord a => [a] -> Map a
mkMap xs = M (nub (sort xs))
