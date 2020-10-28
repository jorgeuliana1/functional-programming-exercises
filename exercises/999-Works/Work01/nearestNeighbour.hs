module NearestNeighbour where
import TupleMath
import DataSet
import DataSetCategories

{-
Gets a list of the distance of the given vector (tuple) to a list of vectors (tuple) in R4.
# Input
dataInput :: IrisDataInput (Vector to be used as reference)
dataSet :: IrisDataSet (List of vectors to have the distance measured)
# Ouput
distances :: [Float] (Distances from dataInput to the vectors of dataSet.
                      each index in `distances` is correspondent to the
                      same index in `dataSet`)
-}
neighboursDistance :: IrisDataInput -> IrisDataSet -> [Float]
neighboursDistance dataInput dataSet = [ euclideanDistance dataInput (a, b, c, d) | (a, b, c, d, _) <- dataSet ]

{-
Returns the index of the nearest neighbour.
# Input
dataInput :: IrisDataInput (Vector to be used as reference)
dataSet :: IrisDataSet (List of vectors to have the distance measured)
# Ouput
index :: Int (Nearest neighbour index in `dataSet`)
-}
nearestNeighbourIndex :: IrisDataInput -> IrisDataSet -> Int
nearestNeighbourIndex dataInput dataSet =
    head (take 1 [ index | index <- [0..(length dataSet) - 1],
                 (distances !! index) == minimum distances ])
                 where
                     distances = neighboursDistance dataInput dataSet

{-
Returns the accuracy of the nearest neighbour model.
# Input
testDataSet :: IrisDataSet (Test data set)
trainDataSet :: IrisDataSet (Train data set)
# Ouput
accuracy :: Float (Accuracy of the model, measures the percentage of correct predictions.
                   Varies from 0 to 1.)
-}
nearestNeighbourAccuracy :: IrisDataSet -> IrisDataSet -> Float
nearestNeighbourAccuracy testDataSet trainDataSet = sum correctPredictions
    where
        correctPredictions =
             [1.00 / testSetLength
             | (a, b, c, d, category) <- testDataSet,
             category == (trainSetCategory (nearestNeighbourIndex (a, b, c, d) trainDataSet))]
        trainSetCategory index = [category | (_, _, _, _, category) <- trainDataSet] !! index
        testSetLength = read (show (length testDataSet)) :: Float