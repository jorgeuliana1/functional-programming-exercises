module Evaluation.CrossValidation where
import Math.Vector
import Math.Matrix
import DataSet.Types
import DataSet.Split

type ValidationFunction = (IrisDataSet -> IrisDataSet -> [IrisCategory])

crossValidateIteration :: ValidationFunction -> IrisDataSet -> [Int] -> [IrisCategory]
crossValidateIteration validationFunction dataSet testIndexes =
    validationFunction testDataSet trainDataSet
    where
        (trainDataSet, testDataSet) = splitDataSet dataSet testIndexes

crossValidate :: ValidationFunction -> IrisDataSet -> [[Int]] -> [[IrisCategory]]
crossValidate validationFunction dataSet testIndexesList =
    [ crossValidateIteration validationFunction dataSet testIndexes
    | testIndexes <- testIndexesList ]