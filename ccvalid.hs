digits :: Int -> [Int]
digits n = [read [x] | x <- show n]

sumOfDigits :: [Int] -> Int
sumOfDigits = sum . concat . map digits

doubleAlternates :: [Int] -> [Int]
doubleAlternates = concat . map (\[f,s] -> [f,2*s]) . pairs

pairs :: [Int] -> [[Int]]
pairs [] = []
pairs xs = take 2 xs : (pairs $ drop 2 xs)

validate :: Int -> Bool
validate n =
  let dblAltRev = doubleAlternates . reverse . digits
      tally = sumOfDigits . dblAltRev 
  in tally n `mod` 10 == 0

