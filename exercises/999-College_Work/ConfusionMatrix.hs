module ConfusionMatrix where
import DataSet.Types

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
confusionMatrix :: [[Int]] (the confusion matrix)
-}
confusionMatrix :: [IrisCategory] -> [IrisCategory] -> [IrisCategory] -> [[Int]]
confusionMatrix cats eOut pOut = [ [ categoriesConfusionCount j i eOut pOut | j <- cats ]
                                 | i <- cats ]
