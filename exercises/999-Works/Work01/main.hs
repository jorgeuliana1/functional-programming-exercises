import ReadCSV
import SplitData
import System.Random

-- The main function currently only serves testing purposes.
main = do
    -- Loading and splitting dataset:
    dataSet <- parseIrisDataFromCSVFile "iris.csv"
    initializeRandomSettings 10
    testSetIndexes <- getTestSetIndexes 10 0.5
    let (trainSet, testSet) = splitDataSet dataSet testSetIndexes
    print trainSet
    print testSet