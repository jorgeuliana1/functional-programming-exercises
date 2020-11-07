module SimplifiedIO where
import System.IO

getInput :: String -> IO String
getInput message = do
    putStr message
    hFlush stdout
    result <- getLine
    return result

fixedDecimalCasesNum :: Double -> Int -> String
fixedDecimalCasesNum n cases = take numChars ((show n) ++ ['0' | i <- [0..]])
    where
        numChars = (length $ show $ floor n) + cases + 1

decimalToPercentage :: Double -> String
decimalToPercentage n = (fixedDecimalCasesNum (n * 100) 2) ++ "%"

spacedInt :: Int -> Int -> String
spacedInt biggestNum num =
    reverse $ take (length $ show biggestNum) ((reverse $ show num) ++ [' ' | i <- [0..]])

stringifyVector :: [String] -> Int -> String -> String
stringifyVector [] vectorL str = str
stringifyVector (x:xs) vectorL str
    | vectorL == 1 = str ++ "," ++ x
    | str == "" = stringifyVector xs (vectorL - 1) (str ++ x)
    | otherwise = stringifyVector xs (vectorL - 1) (str ++ "," ++ x)

alignedVectorStringInt :: Int -> [Int] -> String
alignedVectorStringInt biggestNum vector = stringifyVector spacedNumbers (length spacedNumbers) ""
    where
        spacedNumbers = map (spacedInt biggestNum) vector

stringifyMatrix :: [String] -> String -> String
stringifyMatrix [] str = str
stringifyMatrix (x:xs) str = stringifyMatrix xs (str ++ x ++ "\n")

alignedMatrixStringInt :: [[Int]] -> Int -> String
alignedMatrixStringInt matrix biggestNum = stringifyMatrix vectorsList ""
    where
        vectorsList = map (alignedVectorStringInt biggestNum) matrix