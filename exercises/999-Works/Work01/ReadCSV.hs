module ReadCSV where

{--
Takes a string and splits it in sections divided by a separator.
# Input:
str :: String (String to be splitted.)
sep :: Char   (The separator to be used as reference.)
# Output:
strs :: [String] (Splitted strings)
--}
splitString :: String -> Char -> [String]
splitString [] sep = [""]
splitString (currChar:restStr) sep
    | currChar == sep = "":rest
    | otherwise = (currChar:(head rest)):(tail rest)
    where
        rest = splitString restStr sep

{-
Reads a CSV file and returns the data.
# Input:
fp :: FilePath (Path of the CSV file.)
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
    return [(splitString csvLine ',') | csvLine <- csvLines]
