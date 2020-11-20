module NearestNeighbour where
import Math.Vector
import DataSet.Types
import DataSet.Categories

{-
Gets a list of the distance of the given vector (vector) to a list of vectors (vector) in R4.
# Input
dataInput :: IrisDataInput (Vector to be used as reference)
dataSet :: IrisDataSet (List of vectors to have the distance measured)
# Ouput
distances :: [Float] (Distances from dataInput to the vectors of dataSet.
                      each index in `distances` is correspondent to the
                      same index in `dataSet`)
-}
neighboursDistance :: IrisDataInput -> IrisDataSet -> [Double]
neighboursDistance dataInput dataSet = [ vectorsEuclideanDistance dataInput inputs | (inputs, _) <- dataSet ]

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
Returns a list of the predicted classes for the test data set.
# Input
testDataSet :: IrisDataSet (Test data set)
trainDataSet :: IrisDataSet (Train data set)
# Ouput
predictions :: [IrisCategory] (List of predictions using the train data set as "model"
                               and the test data set as data input.)
-}
predictDataSetNNeighbour :: IrisDataSet -> IrisDataSet -> [IrisCategory]
predictDataSetNNeighbour testDataSet trainDataSet = [ category | (inputs, category) <- nNeighbours ]
    where
        nNeighbours = [ trainDataSet !! index | index <- nnIndexes ]
        nnIndexes = [ nearestNeighbourIndex inputs trainDataSet | (inputs, _) <- testDataSet]