import qualified Data.Char as Char

-- 1. Given a sentence, define a function called capitalise which returns the same sentence with all words capitalised except the ones from a given list. (source: https://www.fer.unizg.hr/)
-- > capitalise :: String -> [String] -> String
-- > capitalise "this is a sentence." ["a", "is"]
-- "This is a Sentence."
getWord word [] = (word, [])
getWord word (x:xs) | (x == ' ') = (word, xs)
                    | (x /= ' ') = getWord (word ++ [x]) xs

splitter lw [] = lw
splitter lw input = let (word, rest) = getWord "" input
                    in splitter (lw ++ [word]) rest

upper el word = if word `elem` el
                then word
                else Char.toUpper (head word) : (tail word)

combine [] = []
combine [a] = a
combine (x:xs) = x ++ " " ++ (combine xs)

capitalize input = combine (map (upper ["is", "a"]) (splitter [] input))

-- 2. Write a definition of composition function (c​ompose​) similar to the compose/dot operator ( •​ ​) in Haskell prelude,
-- that accept two functions and return a new function. Define the type of the c​ ompose​ function.
composeFunction :: (gx -> fx) -> (g -> gx) -> g -> fx
composeFunction f g x = f (g x)

-- 3. Definisikan fungsi ​last​, dengan hanya menggunakan f​ oldr​ atau ​foldl.​ ​ Fungsi ​last tersebut menerima sebuah list dan
-- mengembalikan elemen terakhir dari list tersebut.
last ls = foldl (\x y -> y) 0 ls

-- 4. Sebagaimanamaterikuliahterkait​ComposingContract,​diperlihatkansebuah contract​ yang disebut Z​ ero-Coupon Bound (z​ cb​).​
-- Pemanggilan fungsi ​zcb t x k,​ menyatakan bahwa pada waktu ​t,​ ​contract​ ini akan senilai dengan ​x​ pada kurs ​k​.
-- Misalkan fungsi contract definisi lain sudah tersedia. Bagaimana mengkomposisikan nya untuk mendefinisikan fungsi z​ cb​.
-- Fungsi yang boleh anda gunakan adalah antara lain: (w​ hen, give, and, or, at, scale, konst, one, zero)​
-- zcb ​::​ ​Date​ ​->​ D​ ouble​ -​ >​ ​Currency​ ​->​ ​Contract
-- zcb t x k = when (at t) (scale (one k))

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

