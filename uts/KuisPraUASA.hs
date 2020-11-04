-- 1. Given a list of words, remove from the list all those which contain four or more vowels and those which have same letter appearing twice (or more) in a row.
-- In addition, any word containing numbers should have the numbers removed. Note that the number removal should happen before any other operations so that
-- the subsequent operations can remove the word if necessary.
-- > weirdFilter :: [String] -> [String]
-- > weirdFilter ["abc", "bananae", "fuzzy", "c1c2"]
-- ["abc"]
notNum x = x `notElem` "1234567890"
removeNum = filter notNum

doesNotHaveDuplicates (x:xs:xss) | (x==xs) = False
doesNotHaveDuplicates (x:xss) = doesNotHaveDuplicates xss
doesNotHaveDuplicates [] = True

isVowel x = x `elem` "aiueoAIUEO"
countVowel = length . filter isVowel
isThreeOrLess x = countVowel x < 4

weirdFilter = filter isThreeOrLess . filter doesNotHaveDuplicates . map removeNum

-- 2. Write a function rotabc that changes a's to b's, b's to c's and c's to a's in a string. Only lowercase letters are affected
rotabc :: String -> String
rotabc = map abc
  where abc 'a' = 'b'
        abc 'b' = 'c'
        abc 'c' = 'a'
        abc  x  =  x

-- 3. Definisikan fungsi last, dengan menerapkan point-free style. Fungsi last tersebut menerima sebuah list dan mengembalikan elemen terakhir dari list tersebut.
last' :: (Num a) => [a] -> a
last' = foldr1 (\x acc -> acc)

-- 4. Sebagaimana materi kuliah terkait Composing Contract, diperlihatkan sebuah contract yang disebut Zero-Coupon Bound (zcb).
-- Pemanggilan fungsi zcb t x k, menyatakan bahwa pada waktu t, contract ini akan senilai dengan x pada kurs k.
-- Misalkan fungsi contract definisi lain sudah tersedia. Bagaimana mengkomposisikan nya untuk mendefinisikan fungsi zcb.
-- Fungsi yang boleh anda gunakan adalah antara lain: (when, give, and, or, at, scale, konst, one, zero)
-- zcb :: Date -> Double -> Currency -> Contract
-- zcb t x k = when (at t) (scale x (one k))

-- 10. Definisikan konstanta ​True​ dan konstanta ​False​ dalam lambda calculus. Gunakan definisi tersebut untuk mendefinisikan statement ​if-then-else.​
-- True = λxy.x
-- False = λxy.y

-- if P then E1 else E2 (ditulisnya PE1E2)

-- Jika P == true:
-- = PE1E2
-- = (true)E1E2
-- = (λxy.x)E1E2
-- = E1

-- Jika P == false:
-- = PE1E2
-- = (false)E1E2
-- = (λxy.y)E1E2
-- = E2
