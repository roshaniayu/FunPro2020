-- menerima parameter List Integer
listSum :: [Int] -> Int
-- mengembalikan total dari elemen didalam list tersebut
listSum [] = 0
listSum (x:xs) = x + listSum xs