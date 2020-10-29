import DataSetParse
import DataSetSplit
import DataSetCategories
import Centroid
import NearestNeighbour

-- The main function currently only serves testing purposes.
main = do
    -- Loading and splitting dataset:
    dataSet <- parseDataFromCSVFile "../iris.csv"
    initializeRandomSettings 1337
    testSetIndexes <- getTestSetIndexes 150 0.30
    let (trainSet, testSet) = splitDataSet dataSet testSetIndexes
    let (testInput, testOutput) = splitDataSetInputOutput testSet
    let categories = dataSetCategories dataSet

    -- Showing the centroids accuracy:
    let trainCentroids = centroids trainSet categories
    let accuracyCentroids = centroidAccuracy trainCentroids categories testInput testOutput
    print accuracyCentroids

    -- Showing the k-NN method accuracy:
    let predictionsKNN = evaluateDataSetNNeighbour testSet trainSet
    let accuracyKNN = nearestNeighbourAccuracy testSet trainSet
    print (predictionsKNN)
