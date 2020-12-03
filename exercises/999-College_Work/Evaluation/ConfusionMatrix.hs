module Evaluation.ConfusionMatrix where
import DataSet.Types
import Math.Vector
import Math.Matrix

{-
Returns the confusion count of the two categories.
# Input
eCat :: Category (expected category)
pCat :: Category (predicted category)
eOut :: [Category] (expected outputs of the model)
pOut :: [Category] (predicted outputs of the model)
# Output
numConfusion :: Int (Number of occurrencies of that specific confusion)
-}
categoriesConfusionCount :: Category -> Category -> [Category] -> [Category] -> Int
categoriesConfusionCount eCat pCat eOut pOut = sum [ 1 | (e, p) <- (zip eOut pOut), eCat == e, pCat == p]

{-
Returns the confusion matrix of the evaluation.
# Input
cats :: [Category] (data set categories)
eOut :: [Category] (expected outputs of the model)
pOut :: [Category] (predicted outputs of the model)
# Output
confusionMatrix :: Matrix Int (the confusion matrix)
-}
confusionMatrix :: [Category] -> [Category] -> [Category] -> Matrix Int
confusionMatrix cats eOut pOut = Matrix [
                                 Vector [ categoriesConfusionCount j i eOut pOut | j <- cats ]
                                 | i <- cats ]

{-
Returns the sum of the confusion matrix of every fold.
# Input
cats :: [Category] (data set categories)
dataSet :: DataSet (data set)
folds :: [[Int]] (Splitted indexes)
pOuts :: [Category] (predictions)
# Output
confusionMatrix :: Matrix Int (the confusion matrix)
-}
confusionMatrixCrossValidation :: [Category] -> DataSet -> [[Int]] -> [[Category]] -> Matrix Int
confusionMatrixCrossValidation cats dataSet folds pOuts =
    foldr addMatrixes emptyMatrix confusionMatrixes
        where
            emptyMatrix = Matrix [ Vector [ 0 | i <- [1..3] ] | i <- [1..3] ]
            confusionMatrixes = [ confusionMatrix cats (eOut fold) pOut | (pOut, fold) <- (zip pOuts folds) ]
            eOut fold = [ snd $ dataSet !! i | i <- fold ]

