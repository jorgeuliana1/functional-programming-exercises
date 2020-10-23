import ReadCSV
import SplitData
import Centroid
import DataSet
import System.Random

-- The main function currently only serves testing purposes.
main = do
    -- Loading and splitting dataset:
    dataSet <- parseIrisDataFromCSVFile "iris.csv"
    initializeRandomSettings 10
    testSetIndexes <- getTestSetIndexes 150 0.33
    let (trainSet, testSet) = splitDataSet dataSet testSetIndexes
    let categories = dataSetCategories dataSet

    -- Showing the centroids of the DataSet:
    putStrLn "Centroids for the classes:"
    print categories
    putStrLn ""
    putStrLn "Test dataset results:"
    print (centroids testSet categories)
    putStrLn "Train dataset results:"
    print (centroids trainSet categories)
    putStrLn "Full dataset results:"
    print (centroids dataSet categories)