import Data.List

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

