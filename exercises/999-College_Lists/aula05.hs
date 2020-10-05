-- 2. Create functions using lists comprehensions:

-- a. Obtain the first n multiples of 7
multiple7 :: Int -> [Int]
multiple7 n = [i*7 | i <- [1..n]]

-- b. Obtain the first n cubes
cubes :: Int -> [Int]
cubes n = [i^3 | i <- [1..n]]

-- c. Obtain a tuple with the numbers of a list and its cube
cubeTuples :: [Int] -> [(Int, Int)]
cubeTuples xs = [(i, i^3) | i <- xs]

-- d. Obtain the multiplication table from 0 to 9 of n
multTable :: Int -> [(Int, Int, Int)]
multTable n = [(i, n, i*n) | i <- [0..9]]

-- e. List of tuples with indexes of a (n, m) matrix
matLin :: Int -> Int -> [(Int, Int)]
matLin n m  = [(i, j) | i <- [0..n - 1], j <- [0..m - 1]]

-- e. List of tuples with indexes of a (n, m) matrix (must show ordenation by columns)
matCol :: Int -> Int -> [(Int, Int)]
matCol n m  = [(j, i) | i <- [0..m - 1], j <- [0..n - 1]]