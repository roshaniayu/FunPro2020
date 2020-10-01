{-
Roshani Ayu Pranasti
1706026052
Pemrograman Fungsional
Tugas 3
-}

-- 1. Buatlah fungsi ​myCurry​ dan m​yUncurry​ yang memiliki sifat
-- seperti fungsi curry dan uncurry pada library Haskell
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y

-- Saya membuat fungsi add untuk menguji fungsi myCurry
add :: Num a => (a, a) -> a
add (x, y) = x + y

-- 2. Deret bilangan fibonacci adalah serangkaian deret angka sederhana
-- yang susunan angkanya merupakan penjumlahan dari dua angka sebelumnya.
-- Buatlah fungsi fibonacci yang menerima bilangan bulat positif n dan
-- mengembalikan list yang berisi bilangan fibonacci dari 0 sampai n
fibonacci :: Int -> [Int]
fibonacci n = until n fiblist
    where fiblist = 0 : 1 : (zipWith (+) fiblist (tail fiblist))
          until n (x:xs) = if x > n then [] else [x] ++ (until n xs)

-- 3. Buatlah fungsi ​power​ namun hanya dengan menggunakan operasi penjumlahan (+)
power :: Int -> Int -> Int
power x 0 = 1
power x y = multiply x (power x (y-1))
    where multiply x 0 = 0
          multiply x y = x + (multiply x (y - 1))

-- 4. Buatlah fungsi ​sumEven yang menerima list dari bilangan asli dan
-- menjumlahkan bilangan-bilangan genap saja.
sumEven :: [Int] -> Int
sumEven xs = foldl (+) 0 [x | x <- xs, x `mod` 2 == 0]

-- 5. Buatlah sebuah kalkulator investasi sederhana berupa fungsi ​invest​ yang
-- menerima 3 buah parameter, yaitu: - nominal tiap bulan = x
--                                   - return investasi tiap bulan (%) = y
--                                   - durasi (bulan) = n
invest :: Float -> Float -> Int -> Float
invest x y n = (calculator x y n) - x
    where calculator x y 0 = x
          calculator x y n = formula(calculator x y (n-1)) y + x
          formula x y = (x * (1 + (y / 100)))