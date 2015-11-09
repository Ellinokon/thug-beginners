module A3BreadButter where

import Data.List
  (
  intersperse,
  )

mapList :: (a -> b) -> [a] -> [b]
mapList _ []     = []
mapList f (x:xs) = f x : mapList f xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ []     = []
filterList p (x:xs) | p x       = x : filterList p xs
                    | otherwise = filterList p xs

add42List :: [Int] -> [Int]
add42List = mapList (+ 42)

invertBoolList :: [Bool] -> [Bool]
invertBoolList = mapList not

geq42List:: [Int] -> [Int]
geq42List = filterList (> 42)

onlyTrueList :: [Bool] -> [Bool]
onlyTrueList = filter (== True)

times2 :: Int -> Int
times2 = (* 2)

take4 :: [a] -> [a]
take4 = take 4

intersperse0 :: [Int] -> [Int]
intersperse0 = intersperse 0

take4intersperse0 :: [Int] -> [Int]
take4intersperse0 = take4 . intersperse0

t4i0t2 :: [Int] -> [Int]
t4i0t2 = take4intersperse0 . mapList times2

letterT :: String -> String
letterT = filterList (== 't') . filterList (== 'T')

notElemList :: (Eq a) => a -> [a] -> Bool
notElemList a = not . elem a

fold :: (a -> b -> b) -> b -> [a] -> b
fold _ z []     =  z
fold f z (x:xs) =  f x (foldr f z xs)

sumFold :: [Int] -> Int
sumFold = fold (+) 0

lengthFold :: [a] -> Int
lengthFold = fold (const (1 +)) 0

reverseFold :: [a] -> [a]
reverseFold = fold (flip (++) . return) []

appendFold :: [a] -> [a] -> [a]
appendFold = flip (fold (:))

mapFold :: (a -> b) -> [a] -> [b]
mapFold f = fold ((:) . f) []

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold p = fold (\x xs -> if p x then x : xs else xs) []
