newTable :: Int -> [[Bool]]
newTable s = [[True | i <- [0..(s - j - 1)]] | j <- [0..(s - 1)]]

tableToStringInternal :: String -> Int -> [[Bool]] -> String
tableToStringInternal s i [] = s
tableToStringInternal s i (l:ls) =
    tableToStringInternal newS (i + 1) ls
    where
        newS = s ++ (show i) ++ ": " ++ markedSigns ++ "\n"
        markedSigns = [ if k then '*' else ' ' | k <- prettyL ]
        prettyL = [ m | k <- l, m <- [k, False] ]

tableToString :: [[Bool]] -> String
tableToString t = tableToStringInternal "" 1 t

askThePlayer :: Int -> IO (Int, Int)
askThePlayer n = do
    putStrLn ("Player " ++ (show n))
    putStr "Enter the number of the line: "
    n <- getLine
    putStr "Number of asterisks to be removed: "
    m <- getLine
    return ((read n :: Int), (read m :: Int))

removeAsterisksInternal :: Int -> Int -> Int -> [[Bool]] -> [Bool]
removeAsterisksInternal i j r t
    | i /= j = t !! (i - 1)
    | otherwise = [ True | j <- [1..length (t !! (i - 1)) - r]]

removeAsterisks :: [[Bool]] -> (Int, Int) -> [[Bool]]
removeAsterisks t g = 
    [removeAsterisksInternal i (fst g) (snd g) t | i <- [1..(length t)]]

tableIsEmpty :: [[Bool]] -> Bool
tableIsEmpty [] = True
tableIsEmpty (t:ts) = (t == []) && (tableIsEmpty ts)

playRound :: [[Bool]] -> Int -> IO String
playRound t p = do
    playerGame <- askThePlayer p
    putStrLn ""
    let table = (removeAsterisks t playerGame)
    putStrLn (tableToString table)
    (playGameInternal table p)

playGameInternal :: [[Bool]] -> Int -> IO String
playGameInternal t p = do
    if tableIsEmpty t
        then do
            return ("Player " ++ (show p) ++ " won!")
        else if p == 2
            then do
                (playRound t 1)
            else
                (playRound t 2)

playGame :: [[Bool]] -> IO String
playGame t = do
    (playGameInternal t 2)

main = do
    let table = newTable 5
    putStrLn (tableToString table)
    result <- (playGame table)
    putStrLn result
    