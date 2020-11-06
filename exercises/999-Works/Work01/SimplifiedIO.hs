module SimplifiedIO where

getInput :: String -> IO String
getInput message = do
    putStr message
    result <- getLine
    return result

fixedDecimalCasesNum :: Double -> Int -> String
fixedDecimalCasesNum n cases = take numChars ((show n) ++ ['0' | i <- [0..]])
    where
        numChars = (length $ show $ floor n) + cases + 1

decimalToPercentage :: Double -> String
decimalToPercentage n = (fixedDecimalCasesNum (n * 100) 2) ++ "%"

--alignedMatrixStringInt :: [[Int]] -> String