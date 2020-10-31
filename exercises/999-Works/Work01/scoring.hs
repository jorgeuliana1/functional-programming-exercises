module Scoring where
import DataSet
import VectorMath

-- Comparing the obtained results with the expected results
evaluatePrediction :: [IrisCategory] -> [IrisCategory] -> Double
evaluatePrediction predictedR expectedR = correctPredictionsCount / dataSetLength
    where
        correctPredictionsCount = sum [ 1.00 | i <- [0..(length predictedR) - 1],
                                        (predictedR !! i) == (expectedR !! i) ]
        dataSetLength = read (show (length predictedR)) :: Double