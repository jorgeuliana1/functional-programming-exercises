module Evaluation.CrossValidation where
import Math.Vector
import Math.Matrix
import DataSet.Types
import DataSet.Split

type ValidationFunction = (DataSet -> DataSet -> [Category])

crossValidateIteration :: ValidationFunction -> DataSet -> [Int] -> [Category]
crossValidateIteration validationFunction dataSet testIndexes =
    validationFunction testDataSet trainDataSet
    where
        (trainDataSet, testDataSet) = splitDataSet dataSet testIndexes

crossValidate :: ValidationFunction -> DataSet -> [[Int]] -> [[Category]]
crossValidate validationFunction dataSet testIndexesList =
    [ crossValidateIteration validationFunction dataSet testIndexes
    | testIndexes <- testIndexesList ]