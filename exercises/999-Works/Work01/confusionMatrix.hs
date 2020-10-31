module ConfusionMatrix where
import DataSet

-- Creates a blank confusion matrix
blankConfusionMatrix :: [IrisCategory] -> [[Int]]
blankConfusionMatrix categories = [[ 0 | j <- categories ] | i <- categories ]

{-
Returns the confusion count of the two categories
# Input
eCat :: IrisCategory (expected category)
pCat :: IrisCategory (predicted category)
eOut :: [IrisCategory] (expected outputs of the model)
pOut :: [IrisCategory] (predicted outputs of the model)
# Ouput
numConfusion :: Int (Number of occurrencies of that specific confusion)
-}
categoriesConfusionCount :: IrisCategory -> IrisCategory -> [IrisCategory] -> [IrisCategory] -> Int
categoriesConfusionCount eCat pCat eOut pOut = sum [ 1 | (e, p) <- (zip eOut pOut), eCat == e, pCat == p]

confusionMatrix :: [IrisCategory] -> [IrisCategory] -> [IrisCategory] -> [[Int]]
confusionMatrix cats eOut pOut = [ [ categoriesConfusionCount i j eOut pOut | j <- cats ]
                                 | i <- cats ]