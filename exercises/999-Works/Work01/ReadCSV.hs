module ReadCSV where
import Data.List.Split

{-
Reads a CSV file and returns the data.
# Input:
fp :: FilePath (Path of the CSV file)
# Output:
csvContent :: IO [[String]] (List where each element corresponds to
                             a line on the CSV file, each "line" is
                             a list of strings, each string in that
                             list correspond to a CSV value.)
-}
readCSV :: FilePath -> IO [[String]]
readCSV fp = do
    rawCSV <- readFile fp
    let csvLines = lines rawCSV
    return [(splitOn "," csvLine) | csvLine <- csvLines]
