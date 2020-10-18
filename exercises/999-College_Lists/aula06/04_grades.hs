import System.IO

getGradesInput :: FilePath -> IO [[Float]]
getGradesInput p = do
    fileContent <- readFile p
    let fileLines = lines fileContent
    putStrLn ("\"" ++ p ++ "\"")
    putStrLn fileContent
    putStrLn ""
    return [[read v :: Float | v <- (words l)] | l <- fileLines]

intToFloat :: Int -> Float
intToFloat i = read (show i) :: Float

calculateGrades :: [[Float]] -> [Float]
calculateGrades gs = [ s / l | (s, l) <- (zip gradesSum gradesLen) ]
    where
        gradesSum = [sum g | g <- gs]
        gradesLen = [intToFloat (length g) | g <- gs]

floatListToTxtInternal :: String -> [Float] -> String
floatListToTxtInternal s [] = s
floatListToTxtInternal s (f:fs) = floatListToTxtInternal (s ++ (show f) ++ "\n") fs

floatListToTxt :: [Float] -> String
floatListToTxt xs = floatListToTxtInternal "" xs

main = do
    gradesList <- getGradesInput "grades.txt"
    let averages = calculateGrades gradesList
    let destinationPath = "output.txt"
    putStrLn ("\"" ++ destinationPath ++ "\"")
    let destinationContent = floatListToTxt averages
    putStr destinationContent
    writeFile destinationPath destinationContent