{-
Roshani Ayu Pranasti
1706026052
Pemrograman Fungsional
Tugas 2
-}

import Data.Char

-- 1. Buatlah fungsi sumOfSquares yang menerima sebuah list of integer dan
-- mengembalikan sebuah integer yang merupakan penjumlahan dari kuadrat elemen list input!
sumOfSquares :: [Int] -> Int
sumOfSquares [] = 0
sumOfSquares (x:xs) = x ^ 2 + sumOfSquares xs

-- 2. Bilangan triangular adalah penjumlahan bilangan positif tersebut dengan seluruh bilangan bulat positif sebelumnya.
-- Contohnya bilangan triangular ke-5 adalah 5+4+3+2+1.
-- Buatlah fungsi triangular yang menerima bilangan bulat positif n dan mengembalikan bilangan triangular yang ke-n!
triangular :: Int -> Int
triangular 0 = 0
triangular x = x + triangular (x-1)

-- 3. Buatlah fungsi power tanpa menggunakan fungsi pangkat yang sudah ada di Haskell,
-- input dibatasi hanya untuk bilangan bulat positif!
power :: Int -> Int -> Int
power x 0 = 1
power x y = x * power x (y-1)

-- 4. Palindrome adalah kata yang dibaca sama dari depan ataupun belakang.
-- Contohnya “Madam, I’m Adam”, “No lemon, no melon” dan lain-lain.
-- Buatlah sebuah fungsi yang menerima string dan mengembalikan Boolean untuk mengecek apakah string tersebut palindrome atau tidak.
-- Referensi: https://stackoverflow.com/questions/40143900/how-to-convert-a-string-to-lowercase-using-lambda-expressions
--            https://stackoverflow.com/questions/30242668/remove-characters-from-string-in-haskell
--            https://stackoverflow.com/questions/7656694/reversing-a-string-or-list-recursively
isPalindrome :: String -> Bool
isPalindrome x = y == reverseString y
    where
        y = map toLower (filter (not . (`elem` ",.?!-:;\"\' ")) x)
        reverseString [] = []
        reverseString (x:xs) = reverseString xs ++ [x]