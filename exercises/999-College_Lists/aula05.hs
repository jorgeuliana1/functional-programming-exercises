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

-- f. List of tuples with indexes of a (n, m) matrix (must show ordenation by columns)
matCol :: Int -> Int -> [(Int, Int)]
matCol n m  = [(j, i) | i <- [0..m - 1], j <- [0..n - 1]]

-- g. List of tuples with indexes of a (l, n, m) matrix
indMat3D :: Int -> Int -> Int -> [(Int, Int, Int)]
indMat3D l n m  = [(h, i, j) | h <- [0..l-1], i <- [0..n - 1], j <- [0..m - 1]]

-- h. Obtain the multiplication table from the first n non-negative numbers:
multiTable :: Int -> [(Int, Int, Int)]
multiTable n = [(i, j, i * j) | j <- [0..n], i <- [0..9]]

-- i. Create a list of lists, where each list is the multiplication table of a number:
multiTableLL :: Int -> [[(Int, Int, Int)]]
multiTableLL n = [[(i, j, i * j) | i <- [0..9]] | j <- [0..n]]

-- i. Obtain the multiplication table of two lists:
multiTableCL :: [Int] -> [Int] -> [(Int, Int, Int)]
multiTableCL xs ys = [(i, j, i * j) | i <- xs, j <- ys]

-- j. Obtain the indexes of a triangular matrix, line by line.
indTriSup :: Int -> Int -> [(Int, Int)]
indTriSup n m = [(i, j) | i <- [0..n-1], j <- [i..m-1]]

-- k. Obtain the indexes of a triangular matrix, line by line.
indTriInf :: Int -> Int -> [(Int, Int)]
indTriInf n m = [(i, j) | i <- [0..n-1], j <- [0..i]]

-- l. Obtain the combinations of a pythagorean triangle with sides smaller than n.
pythagoras :: Int -> [(Int, Int, Int)]
pythagoras n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a ^ 2 + b ^ 2 == c ^ 2]

-- m. Present the factors of an Integer positive number.
factors :: Int -> [Int]
factors n = [i | i <- [1..n], n `mod` i == 0]

-- n. Obtain prime numbers smaller than n.
primes :: Int -> [Int]
primes n = [i | i <- [2..n], length ([j | j <- [2..i - 1], i `mod` j == 0]) == 0]

-- o. Obtain perfect numbers smaller than n.
perfects :: Int -> [Int]
perfects n = [i | i <- [1..n], sum(tail(reverse(factors i))) == i]

-- p. Count the occurrences of an element in a list.
countElem :: (Eq a) => a -> [a] -> Int
countElem x xs = length [e | e <- xs, e == x]

-- q. Verify if a list is ordered.
order :: (Ord a) => [a] -> Bool
order xs =
    length [ True | i <- [0..length xs - 1], (length [j | j <- take i xs, xs !! i > j]) == (length (take i xs))] == length xs

-- r. Calculate the scalar product of two vectors in the same dimension.
scalar :: [Int] -> [Int] -> Int
scalar xs ys = sum [xs !! i * ys !! i | i <- [0..length xs - 1]]

-- s. Obtain the positions where elements are found in a list.
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | i <- [0..length xs - 1], xs !! i == x]

-- t. Generate a list containing the first prime numbers.
nPrimesLazy :: Int -> [Int]
nPrimesLazy n = take n [i | i <- [2..], length ([j | j <- [2..i - 1], i `mod` j == 0]) == 0]