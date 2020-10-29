module DataSetParse where
import ReadCSV
import DataSet

{-
Just a "syntatic sugar", it is used to reduce the "size" of the castIrisData function.
# Input
str :: String (String representing a Doubleing-point number)
# Output
num :: Double (The parsed value of str)
-}
toDouble :: String -> Double
toDouble str = read str :: Double

{-
Casts the string-formated data to IrisDataSet-formated data.
# Input
d :: [[String]] (CSV file represation in a list of list of strings)
# Output
irisDataSet :: IrisDataSet (The correctly-formated data set)
-}
castData :: [[String]] -> IrisDataSet
castData d = [ ([toDouble(j) | j <- (init i)], (last i)) | i <- d ]

{-
This function combines the readCSV function with the castData function.
# Input
fp :: FilePath (The CSV file path)
# Output
irisDataSet :: IO IrisDataSet (The correctly-formated data set)
-}
parseDataFromCSVFile :: FilePath -> IO IrisDataSet
parseDataFromCSVFile fp = do
    csvData <- readCSV fp
    return (castData csvData)