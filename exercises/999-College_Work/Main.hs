import System.IO (writeFile)
import Text.Printf (printf)
import Math.Vector
import DataSet.Parse
import DataSet.Split
import DataSet.Categories
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
    dataSet <- parseDataFromCSVFile dataSetCSVPath
    initializeRandomSettings (read givenSeedStr :: Int)
    dataSetIndexes <- getTestSetIndexes (length dataSet) 1
    let foldedIndexes = kFold (read foldsNumStr :: Int) dataSetIndexes
    let categories = dataSetCategories dataSet

    -- Showing the centroids accuracy:
    let centroidValidation = crossValidate $ trainAndPredictDataSetCentroid categories
    let predictionsCentroids = centroidValidation dataSet foldedIndexes
    let accuraciesCentroids = evaluatePredictions predictionsCentroids dataSet foldedIndexes
    let accuracyCentroids = vectorSimpleAverage accuraciesCentroids
    let sDeviationCentroids = vectorStandardDeviation accuraciesCentroids

    -- Showing the 1-NN method accuracy:
    let nnValidation = crossValidate $ predictDataSetNNeighbour 1
    let predictionsNN = nnValidation dataSet foldedIndexes
    let accuraciesNN = evaluatePredictions predictionsNN dataSet foldedIndexes
    let accuracyNN = vectorSimpleAverage accuraciesNN
    let sDeviationNN = vectorStandardDeviation accuraciesNN

    -- Showing the k-NN method accuracy:
    let knnValidation = crossValidate $ predictDataSetNNeighbour (read neighboursNumStr :: Int)
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
    --let kNNMatrix = confusionMatrix categories testOutput predictionsKNN
    let kNNMatrixStr = ""--alignedMatrixStringInt kNNMatrix (length dataSet)
    --let centroidsMatrix = confusionMatrix categories testOutput predictionsCentroids
    let centroidsMatrixStr = ""--alignedMatrixStringInt centroidsMatrix 100 -- Fixed the maximum value to 100 to better fit in the work requirements.
    let fileContents = "vizinho mais próximo:\n" ++ kNNMatrixStr ++ "\n" ++ "centroides:\n" ++ centroidsMatrixStr
    writeFile outputTxtPath fileContents
