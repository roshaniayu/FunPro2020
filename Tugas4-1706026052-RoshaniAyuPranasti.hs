{-
Roshani Ayu Pranasti
1706026052
Pemrograman Fungsional
Tugas 4
-}

-- 1. Buatlah definisi fungsi length baru menggunakan map dan fold!
length' :: Num a => [b] -> a
length' xs = foldl1 (+) (map (\_ -> 1) xs)

-- 2. Diberikan fungsi addUp ns = filter greaterOne (map addOne ns) dimana greaterOne
-- n = n > 1 dan addOne n = n + 1, definisikan ulang fungsi tersebut dengan filter
-- sebelum map, misalnya addUp ns = map fun1 (filter fun2 ns)!
addUp :: (Num b, Ord b) => [b] -> [b]
addUp ns = map (+1) (filter (>0) ns)

-- 3. Definisikan fungsi sum of the squares dari 1 sampai n dengan cara berikut!
-- a. map dan fold
sumOfSquares1 :: (Num a, Enum a) => a -> a
sumOfSquares1 n = foldl1 (+) (map (^2) [1..n])
-- b. fold dan listcomprehension
sumOfSquares2 :: (Num a, Enum a) => a -> a
sumOfSquares2 n = foldl1 (+) [x^2 | x <- [1..n]]

-- 4. Definisikan fungsi yang mengembalikan jumlah bilangan kelipatan 5 dalam sebuah list!
multipleOf5 :: (Num a1, Integral a2) => [a2] -> a1
multipleOf5 xs = foldl1 (+) (map (\_ -> 1) (checkMultipleOf5 xs))
    where checkMultipleOf5 = filter (\x -> x `mod` 5 == 0)

-- 5. Definisikan fungsi total dimana total :: (Int -> Int) -> (Int -> Int) sehingga
-- total f adalah fungsi ketika mendapat nilai n memberikan total dari f 0 + f 1 + ... + f n!
total :: (Int -> Int) -> (Int -> Int)
total f n = foldl1 (+) (map f [0..n])

-- 6. Buatlah fungsi reverse dengan menggunakan foldr!
reverse' :: Foldable t => t a -> [a]
reverse' xs = foldr (\x acc -> acc ++ [x]) [] xs

-- 8. Buatlah definisi infinite list dari triple pythagoras. List tersebut terdiri dari
-- elemen triple bilangan bulat positif yang mengikut persamaan pythagoras x2 + y2 = z2!
pythaTriple :: [(Integer, Integer, Integer)]
pythaTriple = [(x,y,z) | z <- [1..], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2]

-- 10. Buatlah fungsi noUpperAndIdent yang menghapus seluruh karakter kapital dan karakter
-- non-alfabet dari argumen String yang diberikan! (Hint: Gunakan library function elem dan isUpper)
noUpperAndIdent :: [Char] -> [Char]
noUpperAndIdent = filter notUpperAndIdent
    where notUpperAndIdent x = x `elem` "abcdefghijklmnopqrstuvwxyz"