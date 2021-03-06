module Golf where

import Data.List

skipN :: Int -> [a] -> [a]
skipN n [] = []
skipN n (x:xs) = x:skipN n (drop n xs)

skips :: [a] -> [[a]]
skips xs = map (\n -> skipN n . drop n $ xs) [0..length xs - 1]


groupsOf3 :: [a] -> [[a]]
groupsOf3 (x1:x2:x3:xs) = [x1,x2,x3]:groupsOf3 (x2:x3:xs)
groupsOf3 _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima ns = map (\([_,x2,_]) -> x2) . filter (\(xs@[_, x2, _]) -> x2 == maximum(xs)) . groupsOf3 $ ns


counts :: [Integer] -> [Int]
counts xs = map (\n -> count n xs) [0..9]
    where count n = length . filter (\x -> x==n)

histRows :: [Int] -> [String]
histRows cts = 
    let maxCount = maximum cts
    in map (\c -> (replicate c '*') ++ (replicate (maxCount-c) ' ')) cts

histogram :: [Integer] -> String
histogram xs = 
    let histList = reverse . transpose . histRows . counts $ xs
        labelList = ["==========","0123456789"]
        in unlines $ histList ++ labelList
