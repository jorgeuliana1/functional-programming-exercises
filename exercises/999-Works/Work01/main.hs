import DataSetParse
import DataSetSplit
import DataSetCategories
import Centroid
import NearestNeighbour
import Scoring
import ConfusionMatrix

-- The main function currently only serves testing purposes.
main = do
    -- Loading and splitting dataset:
    dataSet <- parseDataFromCSVFile "iris.csv"
    initializeRandomSettings 3
    testSetIndexes <- getTestSetIndexes 150 0.75
    let (trainSet, testSet) = splitDataSet dataSet testSetIndexes
    let (testInput, testOutput) = splitDataSetInputOutput testSet
    let categories = dataSetCategories dataSet

    -- Showing the centroids accuracy:
    let trainCentroids = centroids trainSet categories
    let predictionsCentroids = predictDataSetCentroid trainCentroids categories testInput
    let accuracyCentroids = evaluatePrediction predictionsCentroids testOutput
    print accuracyCentroids

    -- Showing the k-NN method accuracy:
    let predictionsKNN = predictDataSetNNeighbour testSet trainSet
    let accuracyKNN = evaluatePrediction predictionsKNN testOutput
    print accuracyKNN

    print (confusionMatrix categories testOutput predictionsKNN)