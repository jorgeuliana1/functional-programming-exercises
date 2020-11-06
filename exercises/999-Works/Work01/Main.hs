import System.IO
import DataSetParse
import DataSetSplit
import DataSetCategories
import Centroid
import NearestNeighbour
import Scoring
import ConfusionMatrix
import SimplifiedIO

-- The main function currently only serves testing purposes.
main = do
    -- Getting info from the standard input:
    dataSetCSVPath <- getInput "Forneca o nome do arquivo de entrada: "
    outputTxtPath <- getInput "Forneca o nome do arquivo de saida: "
    testCasesPercentageStr <- getInput "Forneca o percentual de exemplos de teste: "
    let testCasesPercentage = (read testCasesPercentageStr :: Float) / 100
    givenSeedStr <- getInput "Forneca o valor da semente para geracao randomizada: "

    -- Loading and splitting dataset:
    dataSet <- parseDataFromCSVFile dataSetCSVPath
    initializeRandomSettings (read givenSeedStr :: Int)
    testSetIndexes <- getTestSetIndexes (length dataSet) testCasesPercentage
    let (trainSet, testSet) = splitDataSet dataSet testSetIndexes
    let (testInput, testOutput) = splitDataSetInputOutput testSet
    let categories = dataSetCategories dataSet

    -- Showing the centroids accuracy:
    let trainCentroids = centroids trainSet categories
    let predictionsCentroids = predictDataSetCentroid trainCentroids categories testInput
    let accuracyCentroids = evaluatePrediction predictionsCentroids testOutput

    -- Showing the k-NN method accuracy:
    let predictionsKNN = predictDataSetNNeighbour testSet trainSet
    let accuracyKNN = evaluatePrediction predictionsKNN testOutput

    -- Printing the accuracy:
    putStrLn ("Acuracia(vizinho): " ++ (decimalToPercentage accuracyKNN))
    putStrLn ("Acuracia(centroide): " ++ (decimalToPercentage accuracyCentroids))
    
    -- Writing confusion matrixes to file:
    let kNNMatrix = confusionMatrix categories testOutput predictionsKNN
    let kNNMatrixStr = alignedMatrixStringInt kNNMatrix (length dataSet)
    let centroidsMatrix = confusionMatrix categories testOutput predictionsCentroids
    let centroidsMatrixStr = alignedMatrixStringInt centroidsMatrix (length dataSet)
    let fileContents = "vizinho mais próximo:\n" ++ kNNMatrixStr ++ "\n" ++ "centroides:\n" ++ centroidsMatrixStr
    writeFile outputTxtPath fileContents
