module Evaluation.ConfusionMatrix where
import DataSet.Types
import Math.Vector
import Math.Matrix

{-
Returns the confusion count of the two categories.
# Input
eCat :: IrisCategory (expected category)
pCat :: IrisCategory (predicted category)
eOut :: [IrisCategory] (expected outputs of the model)
pOut :: [IrisCategory] (predicted outputs of the model)
# Output
numConfusion :: Int (Number of occurrencies of that specific confusion)
-}
categoriesConfusionCount :: IrisCategory -> IrisCategory -> [IrisCategory] -> [IrisCategory] -> Int
categoriesConfusionCount eCat pCat eOut pOut = sum [ 1 | (e, p) <- (zip eOut pOut), eCat == e, pCat == p]

{-
Returns the confusion matrix of the evaluation.
# Input
cats :: [IrisCategory] (data set categories)
eOut :: [IrisCategory] (expected outputs of the model)
pOut :: [IrisCategory] (predicted outputs of the model)
# Output
confusionMatrix :: Matrix Int (the confusion matrix)
-}
confusionMatrix :: [IrisCategory] -> [IrisCategory] -> [IrisCategory] -> Matrix Int
confusionMatrix cats eOut pOut = Matrix [
                                 Vector [ categoriesConfusionCount j i eOut pOut | j <- cats ]
                                 | i <- cats ]

{-
Returns the sum of the confusion matrix of every fold.
# Input
cats :: [IrisCategory] (data set categories)
dataSet :: IrisDataSet (data set)
folds :: [[Int]] (Splitted indexes)
pOuts :: [IrisCategory] (predictions)
# Output
confusionMatrix :: Matrix Int (the confusion matrix)
-}
confusionMatrixCrossValidation :: [IrisCategory] -> IrisDataSet -> [[Int]] -> [[IrisCategory]] -> Matrix Int
confusionMatrixCrossValidation cats dataSet folds pOuts =
    foldr addMatrixes emptyMatrix confusionMatrixes
        where
            emptyMatrix = Matrix [ Vector [ 0 | i <- [1..3] ] | i <- [1..3] ]
            confusionMatrixes = [ confusionMatrix cats (eOut fold) pOut | (pOut, fold) <- (zip pOuts folds) ]
            eOut fold = [ snd $ dataSet !! i | i <- fold ]

