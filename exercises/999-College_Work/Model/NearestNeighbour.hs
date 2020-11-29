module Model.NearestNeighbour where
import Math.Vector
import DataSet.Types
import DataSet.Categories
import Algorithm.Sorting

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
Returns the indexes of the k (given) nearest neighbours.
# Input
dataInput :: IrisDataInput (Vector to be used as reference)
dataSet :: IrisDataSet (List of vector to have the distance measured)
k :: Int (The number of indexes to be returned)
# Output
indexes :: [Int] (The k nearest neighbours indexes in `dataSet`)
-}
kNearestNeighboursIndexes :: IrisDataInput -> IrisDataSet -> Int -> [Int]
kNearestNeighboursIndexes dataInput dataSet k =
    filter (\i -> (currIndexDistance i) `elem` cresDistances) [0..]
    where
        currIndexDistance i = vectorsEuclideanDistance dataInput $ currIndexVector i
        currIndexVector i = fst $ dataSet !! i
        cresDistances {- The k neighbours distances in crescent order -} =
            take k $ quickSort $ neighboursDistance dataInput dataSet

{-
Returns the index of the nearest neighbour.
# Input
k :: Int (Number of nearest neighbours to be considered)
dataInput :: IrisDataInput (Vector to be used as reference)
dataSet :: IrisDataSet (List of vectors to have the distance measured)
# Ouput
index :: Int (Nearest neighbour index in `dataSet`)
-}
nearestNeighbourIndex :: Int -> IrisDataInput -> IrisDataSet -> Int
nearestNeighbourIndex k dataInput dataSet = head $ kNearestNeighboursIndexes dataInput dataSet k

{-
Returns a list of the predicted classes for the test data set.
# Input
k :: Int (Number of nearest neighbours to be considered)
testDataSet :: IrisDataSet (Test data set)
trainDataSet :: IrisDataSet (Train data set)
# Ouput
predictions :: [IrisCategory] (List of predictions using the train data set as "model"
                               and the test data set as data input.)
-}
predictDataSetNNeighbour :: Int -> IrisDataSet -> IrisDataSet -> [IrisCategory]
predictDataSetNNeighbour k testDataSet trainDataSet = [ category | (inputs, category) <- nNeighbours ]
    where
        nNeighbours = [ trainDataSet !! index | index <- nnIndexes ]
        nnIndexes = [ nearestNeighbourIndex k inputs trainDataSet | (inputs, _) <- testDataSet]
