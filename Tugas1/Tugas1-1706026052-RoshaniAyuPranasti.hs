{-
Roshani Ayu Pranasti
1706026052
Pemrograman Fungsional
Tugas 1 Pengenalan Haskell
-}

-- 1.
-- menerima parameter r Double
circleArea :: Double -> Double
-- mengembalikan luas dari lingkaran dengan jari-jari r dengan pi 3.14
circleArea r = 3.14 * r ^ 2

-- 2.
-- menerima 3 buah parameter bertipe Integer
isTriangle :: Int -> Int -> Int -> Bool
-- mengembalikan Boolean yang menyatakan apakah segitiga dapat dibuat berdasarkan 3 buah sisi tersebut
isTriangle a b c = not ((a + b <= c) || (a + c <= b) || (b + c <= a))

-- 3.
-- menerima parameter List Integer
listSum :: [Int] -> Int
-- mengembalikan total dari elemen didalam list tersebut
listSum [] = 0
listSum (x:xs) = x + listSum xs

-- 4.
-- menerima parameter List circleArea
listSumArea :: [Double] -> Double
-- mengembalikan total dari semua area di dalam list tersebut
listSumArea [] = 0
listSumArea (x:xs) = x + listSumArea xs

-- 5.
-- implementasi reverseList
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- 6.
-- implementasi algoritma quicksort descending
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  biggerSorted ++ [x] ++ smallerSorted