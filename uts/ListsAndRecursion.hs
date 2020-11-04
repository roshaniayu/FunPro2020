-- 1. A recursive function is only sensible if the condition is that the value of its parameters becomes simpler in each recursive application is met.
-- Consider the following definition of the factorial function:
fac n | n == 0 = 1
      | otherwise = n * fac (n - 1)
-- What happens if you evaluate fac (−3)?

-- Exception: stack overflow

-- How can you formulate the condition more precisely?

-- The evaluation of factorial n for any negative number loops indefinitely. The reason is that there is no base case to stop the recursion.
-- > fac (3)
-- 6

-- 2. Here is a definition for exponentiation:
-- x ^ 0 = 1
-- x ^ n = x * (x ^ (n-1))
-- Give an alternative definition for that treats the two cases where n is even and where n is uneven separately.
-- You can exploit the fact that x ^ n = (x ^ (n/2)) ^ 2
pangkat _ 0 = 1
pangkat x 1 = x
pangkat x n
  | n `mod` 2 == 0 = y * y
  | otherwise = x * y * y
  where y = pangkat x (n `quot` 2)

-- Which intermediate results are being computed for the computation of 2 ^ 10 in the old and the new definition?

-- aaa

-- 3. Define a function which returns the last element of a list.
last' :: [a] -> a
last' (x:[]) = x
last' (_:xs) = last' xs
-- atau
lastElement :: [a] -> a
lastElement = last

-- 4. Define a function that returns the one but last element of a list.
init' :: [a] -> [a]
init' (_:[]) = []
init' (x:xs) = x:init' xs
-- atau
initElement :: [a] -> [a]
initElement = init

-- 5. Define an operator (!!) which returns the i-th element of a list
getIndex (x:_) 0 = x
getIndex (_:xs) index = getIndex xs (index-1)

-- 6. Define a function that determines whether a list is a palindrome, that is, whether the list is equal to its reversal.
palindrome' x = x == reverseList x
  where reverseList [] = []
        reverseList (x:xs) = reverseList xs ++ [x]
-- atau
isPalindrome x = x == reverse x

-- 7. Define the function concat :: [[a]] −> [a] which flattens a list of lists: concat [[1, 2], [3], [ ], [4, 5]] evaluates to [1, 2, 3, 4, 5].
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- 8. Define a function remSuccessiveDuplicates which removes succesive repeated elements from a list: [1, 2, 2, 3, 2, 4] is mapped to [1, 2, 3, 2, 4].
remSuccessiveDuplicates [] = []
remSuccessiveDuplicates (x:[]) = x:[]
remSuccessiveDuplicates (x:y:xs)
  | x == y = remSuccessiveDuplicates (x:xs)
  | otherwise = x:remSuccessiveDuplicates (y:xs)

-- 9. Define a function that groups successive duplicate elements in a list into sublists: [1, 2, 2, 3, 2, 4] is mapped to [[1], [2, 2], [3], [2], [4]].
groupSuccessiveDuplicates [] = []
groupSuccessiveDuplicates (x:[]) = [x]:[]
groupSuccessiveDuplicates (x:y:xs)
  | x == y = [x:y:[]] ++ groupSuccessiveDuplicates xs
  | otherwise = [x]:groupSuccessiveDuplicates (y:xs)

-- 10. Define a function that determines the “run-length encoding” of a list: [1, 2, 2, 3, 2, 4] is mapped to [(1, 1),(2, 2),(1, 3),(1, 2),(1, 4)]. That is, the list is mapped to a list of pairs whose first element says how many times the second component of the pair appears in adjacent positions in the list.
-- Define a function which constructs the original list given its run-length-encoded version.
runLengthEncodingSuccessiveDuplicates x = runLengthEncodingSuccessiveDuplicates' $ groupSuccessiveDuplicates x
  where runLengthEncodingSuccessiveDuplicates' [] = []
        runLengthEncodingSuccessiveDuplicates' (y:ys) = (length y, head y):runLengthEncodingSuccessiveDuplicates' ys

-- 11. Verify that the definition of (++) indeed maps [1, 2] ++ [] to [1, 2]. Hint: write [1, 2] as 1 : (2 : [ ]).

-- aaa

-- 12. Which of the following expressions returns True for all lists xs, and which False?
-- [[ ]] ++ xs   == xs (False)
-- [[ ]] ++ xs   == [xs] (False)
-- [[ ]] ++ xs   == [[ ], xs] (False)
-- [[ ]] ++ [xs] == [[ ], xs] (True)
-- [xs]  ++ [ ]  == [xs] (True)
-- [xs]  ++ [xs] == [xs, xs] (True)

-- 13. Write a function which takes two lists and removes all the elements from the second list from the first list. (This function is defined in Data.List as (\\).)

-- aaa

-- 14. We can represent a matrix as a list of lists of the same length. Write a function transpose :: [[a]] -> [[a]] which maps the i-th element of the j-th list to the j-th element of the i-th list. Hint: make use of the function:
-- zipWith op (x:xs) (y:ys) = (x `op` y) : zipWith xs ys
-- zipWith op _      _      = [ ]

-- aaa

-- 15. Implement the function split with the following type: split :: Int -> [a] -> [[a]]. This function divides the given list in sublists, where the sublists have the given length. Only the last list might be shorter. The function can be used as follows:
-- > split 3 [1..11]
-- [[1,2,3],[4,5,6],[7,8,9],[10,11]]

-- aaa

-- 16. Write a function gaps that gives all the possibilities to take out one element from a list. For example:
-- gaps [1,2,3,4,5] = [[2,3,4,5], [1,3,4,5], [1,2,4,5], [1,2,3,5], [1,2,3,4]]