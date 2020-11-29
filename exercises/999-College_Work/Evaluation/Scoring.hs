module Evaluation.Scoring where
import DataSet.Types
import DataSet.Split
import Math.Vector

{--
This function compares the obtained results with the expected results and returns
the numbers of correct predictions over the total number of predictions.
# Input:
predictedR :: [IrisCategory] (Predicted results.)
expectedR  :: [IrisCategory] (Expected results.)
# Output:
accuracy :: Double (The accuracy of the predictions.)
--}
evaluatePrediction :: [IrisCategory] -> [IrisCategory] -> Double
evaluatePrediction predictedR expectedR = correctPredictionsCount / dataSetLength
    where
        correctPredictionsCount = sum [ 1.00 | i <- [0..(length predictedR) - 1],
                                        (predictedR !! i) == (expectedR !! i) ]
        dataSetLength = read (show (length predictedR)) :: Double

evaluatePredictions :: [[IrisCategory]] -> IrisDataSet -> [[Int]] -> Vector Double
evaluatePredictions predictedRs dataSet foldedIndexes =
    Vector [ evaluatePrediction predictedR (expectedR is)
           | (predictedR, is) <- (zip predictedRs foldedIndexes) ]
    where
        expectedR indexes = snd $ splitDataSetInputOutput $ snd $ splitDataSet dataSet indexes
