module ReadCSV where
import Data.List.Split
import DataSet

-- Reads a CSV file and returns the data (the data is organized but not parsed).
readCSV :: FilePath -> IO [[String]]
readCSV fp = do
    rawCSV <- readFile "iris.csv"
    let csvLines = lines rawCSV
    return [(splitOn "," csvLine) | csvLine <- csvLines]

-- Just a "syntatic sugar", it is used to reduce the "size" of the castIrisData function.
toFloat :: String -> Float
toFloat str = read str :: Float

-- This function has a really specific usage, it is only useful in the work's context.
-- It will convert the "raw data" to a more "computer-oriented" version.
castIrisData :: [[String]] -> IrisDataSet
castIrisData d = [ (toFloat(i!!0), toFloat(i!!1), toFloat(i!!2), toFloat(i!!3), i!!4) | i <- d ]

-- Another "work-oriented" function, it returns the ready-to-be-used data.
parseIrisDataFromCSVFile :: FilePath -> IO IrisDataSet
parseIrisDataFromCSVFile fp = do
    csvData <- readCSV fp
    return (castIrisData csvData)