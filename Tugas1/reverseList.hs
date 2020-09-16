-- implementasi reverseList
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]