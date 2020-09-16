circleArea :: Double -> Double
circleArea r = 3.14 * r ^ 2

-- menerima parameter List circleArea
listSumArea :: [Double] -> Double
-- mengembalikan total dari semua area di dalam list tersebut
listSumArea [] = 0
listSumArea (x : xs) = x + listSumArea xs