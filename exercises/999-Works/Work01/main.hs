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
    testSetIndexes <- getTestSetIndexes 150 0.50
    let (trainSet, testSet) = splitDataSet dataSet testSetIndexes
    let (testInput, testOutput) = splitDataSetInputOutput testSet
    let categories = dataSetCategories dataSet

    -- Showing the centroids accuracy:
    let trainCentroids = centroids trainSet categories
    let accuracyCentroids = centroidAccuracy trainCentroids categories testInput testOutput
    print accuracyCentroids