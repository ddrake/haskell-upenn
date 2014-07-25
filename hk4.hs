import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2)*fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x a -> (x-2)*a) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
	| even n = n + fun2 (n `div` 2)
	| otherwise = fun2 (3*n + 1)

collatz :: Integer -> Integer
collatz 1 = 1
collatz n
	| even n = n `div` 2
	| otherwise = 3*n + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate collatz

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show)

-- uses foldr to generate a balanced tree from a list of values
foldTree :: [a] -> Tree a
foldTree = foldr (\x a -> balancedInsert a x) Leaf

-- if the left branch is shorter, insert it there, otherwise insert right
balancedInsert :: Tree a -> a -> Tree a
balancedInsert Leaf n = Node 0 Leaf n Leaf
balancedInsert (Node _ left node right) n
  | hl < hr   = Node ((max nhl hr) + 1) newLeft node right
  | otherwise = Node ((max hl nhr) + 1) left node newRight
  where hl = height left
        hr = height right
        newLeft = balancedInsert left n
        newRight = balancedInsert right n
        nhl = height newLeft
        nhr = height newRight

height :: Tree a -> Integer
height Leaf = -1
height (Node ht _ _ _) = ht

xor :: [Bool] -> Bool
xor = foldr (\x a -> if x then not a else a) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x a -> f x:a) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a xs =  foldr (flip f) a (reverse xs)