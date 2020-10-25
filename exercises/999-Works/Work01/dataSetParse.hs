module DataSetParse where
import ReadCSV
import DataSet

{-
Just a "syntatic sugar", it is used to reduce the "size" of the castIrisData function.
# Input
str :: String (String representing a floating-point number)
# Output
num :: Float (The parsed value of str)
-}
toFloat :: String -> Float
toFloat str = read str :: Float

{-
Casts the string-formated data to IrisDataSet-formated data.
# Input
d :: [[String]] (CSV file represation in a list of list of strings)
# Output
irisDataSet :: IrisDataSet (The correctly-formated data set)
-}
castIrisData :: [[String]] -> IrisDataSet
castIrisData d = [ (toFloat(i!!0), toFloat(i!!1), toFloat(i!!2), toFloat(i!!3), i!!4) | i <- d ]

{-
This function combines the readCSV function with the castIrisData function.
# Input
fp :: FilePath (The CSV file path)
# Output
irisDataSet :: IO IrisDataSet (The correctly-formated data set)
-}
parseIrisDataFromCSVFile :: FilePath -> IO IrisDataSet
parseIrisDataFromCSVFile fp = do
    csvData <- readCSV fp
    return (castIrisData csvData)