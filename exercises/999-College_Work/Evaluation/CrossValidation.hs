module Evaluation.CrossValidation where
import Math.Vector
import Math.Matrix
import DataSet.Types
import DataSet.Split
import DataSet.Standardization

type ValidationFunction = (DataSet -> DataSet -> [Category])

crossValidateIteration :: ValidationFunction -> DataSet -> [Int] -> [Category]
crossValidateIteration validationFunction dataSet testIndexes =
    validationFunction testDataSetS trainDataSetS
    where
        testDataSetS = standardizeDataSet testDataSet trainDataSet
        trainDataSetS = standardizeDataSet trainDataSet trainDataSet
        (trainDataSet, testDataSet) = splitDataSet dataSet testIndexes

crossValidate :: ValidationFunction -> DataSet -> [[Int]] -> [[Category]]
crossValidate validationFunction dataSet testIndexesList =
    [ crossValidateIteration validationFunction dataSet testIndexes
    | testIndexes <- testIndexesList ]