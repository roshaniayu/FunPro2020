-- menerima 3 buah parameter bertipe Integer
isTriangle :: Integer -> Integer -> Integer -> Bool
-- mengembalikan Boolean yang menyatakan apakah segitiga dapat dibuat berdasarkan 3 buah sisi tersebut
isTriangle a b c = not ((a + b <= c) || (a + c <= b) || (b + c <= a))
