module DataSet.Parse (parseDataFromCSVFile) where
import DataSet.Types
import Math.Vector
import ReadCSV

{-
Just a "syntatic sugar", it is used to reduce the "size" of the castData function.
# Input
str :: String (String representing a Doubleing-point number)
# Output
num :: Double (The parsed value of str)
-}
toDouble :: String -> Double
toDouble str = read str :: Double

{-
Casts the string-formated data to DataSet-formated data.
# Input
d :: [[String]] (CSV file represation in a list of list of strings)
# Output
DataSet :: DataSet (The correctly-formated data set)
-}
castData :: [[String]] -> DataSet
castData d = [ ( Vector [toDouble(j) | j <- (init i)], (last i)) | i <- d ]

{-
This function combines the readCSV function with the castData function.
# Input
fp :: FilePath (The CSV file path)
# Output
DataSet :: IO DataSet (The correctly-formated data set)
-}
parseDataFromCSVFile :: FilePath -> IO DataSet
parseDataFromCSVFile fp = do
    csvData <- readCSV fp
    return (castData csvData)
