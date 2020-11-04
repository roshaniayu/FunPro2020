-- 1. Write a function noOfSol that, for some a, b, and c, determines the number of solutions of the equation ax² + bx + c = 0, using case distinction.

-- The number of solutions is given by the result of the discriminant, b² - 4ac.
noOfSol :: Int -> Int -> Int -> Int
noOfSol a b c | discr >  0 = 2
              | discr == 0 = 1
              | otherwise  = 0
  where discr = b * b - 4 * a * c

-- 2. What is the type of the following functions? tail, sqrt, pi, exp, (ˆ), (/=) and noOfSol? How can you query the interpreter for the type of an
-- expression and how can you explicitly specify the types of functions in your program?

-- Using :t to show the type of the following functions:
-- tail :: [a] -> [a]
-- sqrt :: Floating a => a -> a
-- pi :: Floating a => a
-- exp :: Floating a => a -> a
-- (^) :: (Integral b, Num a) => a -> b -> a
-- (/=) :: Eq a => a -> a -> Bool
-- noOfSol :: Int -> Int -> Int -> Int

-- 3. Given the following definitions:
thrice :: a -> [a]
thrice x = [x, x, x]

sums :: Num a => [a] -> [a]
sums (x : y : ys) = x : sums (x + y : ys)
sums xs           = xs
-- What does the following expression evaluate to?
-- map thrice (sums [0 .. 4])

-- The expression evalutes to [[0,0,0],[1,1,1],[3,3,3],[6,6,6],[10,10,10]]

-- Types and inference
-- In these exercises you should assume the following types:
-- foldr  :: (a -> b -> b) -> b -> [a] -> b
-- map    :: (a -> b) -> [a] -> [b]
-- concat :: [[a]] -> [a]
-- (.)    :: (b -> c) -> (a -> b) -> a -> c

-- 1. What is the type of foldr map?
types1 :: [a] -> [a -> a] -> [a]
types1 = foldr map

-- 2. What is the type of map . foldr?
types2 :: (b -> a -> a) -> [a] -> [[b] -> a]
types2 = map . foldr

-- 3. Which of the following is the type of concat . concat?
types3 :: [[[a]]] -> [a]
types3 = concat . concat

-- 4. What is the type of map (map map)?
types4 :: [[a -> b]] -> [[[a] -> [b]]]
types4 = map (map map)

-- 5. Which observation is correct when comparing the types of (map map) map and map (map map)?

-- (map map) map doesn't have any type
-- One of the expressions does not have any type at all.