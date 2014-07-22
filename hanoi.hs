type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from _ to = [(from, to)]
hanoi n from temp to =
  let firstMoves = hanoi (n-1) from to temp
      lastMoves = hanoi (n-1) temp from to
  in firstMoves ++ (from, to):lastMoves

hanoi4' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4' 1 from _ _ to = [(from, to)]
hanoi4' 2 from t1 _ to = hanoi 2 from t1 to
hanoi4' 3 from t1 t2 to = [(from, t1),(from, t2),(from, to),(t2, to), (t1, to)]
hanoi4' n from t1 t2 to = 
  let third = (n-1) `div` 3
      rest = n-1-third
      firstMoves = hanoi4' rest from t2 to t1
      secondMoves = hanoi third from to t2 
      thirdMoves = hanoi third t2 from to
      lastMoves = hanoi4' rest t1 from t2 to 
  in  firstMoves ++ secondMoves ++ [(from, to)] ++ thirdMoves ++ lastMoves

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 from _ _ to = [(from, to)]
hanoi4 2 from t1 _ to = hanoi 2 from t1 to
hanoi4 n from t1 t2 to = 
  let half = (n-1) `div` 2
      rest = n-1-half
      firstMoves = hanoi4 half from t2 to t1
      secondMoves = hanoi rest from t2 to 
      lastMoves = hanoi4 half t1 from t2 to 
  in  firstMoves ++ secondMoves ++ lastMoves

hanoi4'' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4'' 1 from _ _ to = [(from, to)]
hanoi4'' 2 from t1 _ to = hanoi 2 from t1 to
hanoi4'' 3 from t1 t2 to = [(from, t1),(from, t2),(from, to),(t2, to), (t1, to)]
hanoi4'' n from t1 t2 to = 
  let third = (n-1) `div` 3
      rest = n-1-third
      firstMoves = hanoi4'' rest from t2 to t1
      secondMoves = hanoi third from t2 to 
      lastMoves = hanoi4'' rest t1 from t2 to 
  in  firstMoves ++ secondMoves ++ lastMoves

