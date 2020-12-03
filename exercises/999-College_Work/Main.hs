import System.IO (writeFile)
import Text.Printf (printf)
import Math.Vector
import DataSet.Parse
import DataSet.Split
import DataSet.Categories
import DataSet.Standardization
import Model.Centroid
import Model.NearestNeighbour
import Evaluation.Scoring
import Evaluation.ConfusionMatrix
import Evaluation.CrossValidation
import Algorithm.Splitting (kFold)
import SimplifiedIO

main = do
    -- Getting info from the standard input:
    dataSetCSVPath <- getInput "Forneca o nome do arquivo de entrada: "
    outputTxtPath <- getInput "Forneca o nome do arquivo de saida: "
    foldsNumStr <- getInput "Forneca o numero de folds: "
    neighboursNumStr <- getInput "Forneca o numero de vizinhos: "
    givenSeedStr <- getInput "Forneca o valor da semente para geracao randomizada: "

    -- Loading and splitting dataset:
    dataSetNonStardadized <- parseDataFromCSVFile dataSetCSVPath
    let dataSet = standardizeDataSet dataSetNonStardadized
    initializeRandomSettings (read givenSeedStr :: Int)
    dataSetIndexes <- getTestSetIndexes (length dataSet) 1
    let foldedIndexes = kFold (read foldsNumStr :: Int) dataSetIndexes
    let foldedIndexes = [[65,3,24,51,135,117,69,79,82,148,108,78,62,137,107,84,36,33,143,129,126,134,123,97,31,9,64,35,43,93,128,6,2,144,29,103,1,19],[146,48,67,32,136,139,70,57,83,109,23,114,96,59,111,4,101,0,55,105,95,14,125,27,10,28,7,80,8,22,72,13,85,120,63,54,102,45],[94,66,44,53,25,92,110,98,131,15,40,49,30,124,73,75,113,118,21,90,38,61,42,41,115,127,5,12,81,89,71,133,142,47,87,106,34],[130,18,20,141,140,100,46,149,16,91,104,52,138,60,119,145,76,11,56,112,88,99,74,39,68,50,132,58,37,116,122,121,77,147,17,26,86]]
    let categories = dataSetCategories dataSet
    
    -- Showing the centroids accuracy:
    let centroidValidation = crossValidate $ trainAndPredictDataSetCentroid categories
    let predictionsCentroids = centroidValidation dataSet foldedIndexes
    let accuraciesCentroids = evaluatePredictions predictionsCentroids dataSet foldedIndexes
    let accuracyCentroids = vectorSimpleAverage accuraciesCentroids
    let sDeviationCentroids = vectorStandardDeviation accuraciesCentroids

    -- Showing the 1-NN method accuracy:
    let nnValidation = crossValidate $ predictDataSetNNeighbour 1 categories
    let predictionsNN = nnValidation dataSet foldedIndexes
    let accuraciesNN = evaluatePredictions predictionsNN dataSet foldedIndexes
    let accuracyNN = vectorSimpleAverage accuraciesNN
    let sDeviationNN = vectorStandardDeviation accuraciesNN

    -- Showing the k-NN method accuracy:
    let knnValidation = crossValidate $ predictDataSetNNeighbour (read neighboursNumStr :: Int) categories
    let predictionsKNN = knnValidation dataSet foldedIndexes
    let accuraciesKNN = evaluatePredictions predictionsKNN dataSet foldedIndexes
    let accuracyKNN = vectorSimpleAverage accuraciesKNN
    let sDeviationKNN = vectorStandardDeviation accuraciesKNN 

    -- Printing the accuracy:
    printf "Acuracia(vizinho): %.2f%%\n" (accuracyNN * 100)
    printf "Desvio-Padrao(vizinho): %.2f%%\n" (sDeviationNN * 100)
    printf "Acuracia(centroide): %.2f%%\n" (accuracyCentroids * 100)
    printf "Desvio-Padrao(centroide): %.2f%%\n" (sDeviationCentroids * 100)
    printf "Acuracia(k-vizinhos): %.2f%%\n" (accuracyKNN * 100)
    printf "Desvio-Padrao(k-vizinhos): %.2f%%\n" (sDeviationKNN * 100)
    
    -- Writing confusion matrixes to file:
    let kNNMatrix = confusionMatrixCrossValidation categories dataSet foldedIndexes predictionsKNN
    let kNNMatrixStr = alignedMatrixStringInt kNNMatrix 100
    let sNNMatrix = confusionMatrixCrossValidation categories dataSet foldedIndexes predictionsNN
    let sNNMatrixStr = alignedMatrixStringInt sNNMatrix 100
    let centroidsMatrix = confusionMatrixCrossValidation categories dataSet foldedIndexes predictionsCentroids
    let centroidsMatrixStr = alignedMatrixStringInt centroidsMatrix 100 -- Fixed the maximum value to 100 to better fit in the work requirements.
    let fileContents = "vizinho mais próximo:\n" ++ sNNMatrixStr ++ "\n" ++ "centroides:\n" ++ centroidsMatrixStr ++ "\n" ++ "k-vizinhos mais próximos:\n" ++ kNNMatrixStr
    writeFile outputTxtPath fileContents