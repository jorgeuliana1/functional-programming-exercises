import System.IO (writeFile)
import Text.Printf (printf)
import DataSet.Parse
import DataSet.Split
import DataSet.Categories
import Centroid
import NearestNeighbour
import Scoring
import ConfusionMatrix
import SimplifiedIO

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
    printf "Acuracia(vizinho): %.2f%%\n" (accuracyKNN * 100)
    printf "Acuracia(centroide): %.2f%%\n" (accuracyCentroids * 100)
    
    -- Writing confusion matrixes to file:
    let kNNMatrix = confusionMatrix categories testOutput predictionsKNN
    let kNNMatrixStr = alignedMatrixStringInt kNNMatrix (length dataSet)
    let centroidsMatrix = confusionMatrix categories testOutput predictionsCentroids
    let centroidsMatrixStr = alignedMatrixStringInt centroidsMatrix 100 -- Fixed the maximum value to 100 to better fit in the work requirements.
    let fileContents = "vizinho mais próximo:\n" ++ kNNMatrixStr ++ "\n" ++ "centroides:\n" ++ centroidsMatrixStr
    writeFile outputTxtPath fileContents
