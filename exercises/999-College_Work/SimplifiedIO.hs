module SimplifiedIO where
import System.IO (hFlush, stdout)

{-
Simillar to the `input()` function in Python. Shows an message in the stdout and waits for input.
# Input:
message :: String (Message to be shown in the stdout)
# Output:
result :: IO String (Input from stdin)
-}
getInput :: String -> IO String
getInput message = do
    putStr message
    hFlush stdout
    result <- getLine
    return result

{--
This function inserts spaces before a given number in order to fit it in a predefined space.
# Input:
biggestNum :: Int (Biggest num to be fit in the space.)
num :: Int (The number to be converted to spaced string.)
# Ouput:
spacedString :: String (The spaced 'stringified' number.)
--}
spacedInt :: Int -> Int -> String
spacedInt biggestNum num =
    reverse $ take (length $ show biggestNum) ((reverse $ show num) ++ [' ' | i <- [0..]])

-- Auxiliary function: converts a list of strings into one string of the form "string1, string2, ..., stringN"
stringifyVector :: [String] -> Int -> String -> String
stringifyVector [] vectorL str = str
stringifyVector (x:xs) vectorL str
    | vectorL == 1 = str ++ "," ++ x
    | str == "" = stringifyVector xs (vectorL - 1) (str ++ x)
    | otherwise = stringifyVector xs (vectorL - 1) (str ++ "," ++ x)

-- Auxiliary function: converts a list of int into one string of spaced ints. Eg.: "  1,  2,  3,  4,  5"
alignedVectorStringInt :: Int -> [Int] -> String
alignedVectorStringInt biggestNum vector = stringifyVector spacedNumbers (length spacedNumbers) ""
    where
        spacedNumbers = map (spacedInt biggestNum) vector

-- Auxiliary function: simillar to `stringifyVector` but works in matrixes.
stringifyMatrix :: [String] -> String -> String
stringifyMatrix [] str = str
stringifyMatrix (x:xs) str = stringifyMatrix xs (str ++ x ++ "\n")

{--
stringifyVector + alignedVectorStringInt + stringifyMatrix + alignedMatrixStringInt
The `alignedMatrixStringInt` returns the 'stringified' version of a matrix of Int. The other functions are auxiliary.
# Input:
matrix :: [[Int]] (Matrix to be converted to string.)
biggestNum :: Int (Biggest possible number in the string. To be used as reference in the internal `spacedInt` call)
# Output:
matrixString :: String (The string representation of the matrix.)
--}
alignedMatrixStringInt :: [[Int]] -> Int -> String
alignedMatrixStringInt matrix biggestNum = stringifyMatrix vectorsList ""
    where
        vectorsList = map (alignedVectorStringInt biggestNum) matrix