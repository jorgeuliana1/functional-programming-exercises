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
    filter (\i -> (currIndexDistance i) `elem` cresDistances) [0..((length dataSet) - 1)]
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
Returns the nearest neighbours splitted into data sets.
# Input
k :: Int (Number of nearest neighbours to be considered)
categories :: [IrisCategory] (List of categories of the data set)
dataInput :: IrisDataInput (Vector to be used as reference)
dataSet :: IrisDataSet (List of vectors to have the distance measured)
# Ouput
category :: [IrisDataSet] (List of data sets organized by category)
-}
kNNSamplesByCategory :: Int -> [IrisCategory] -> IrisDataInput -> IrisDataSet -> [IrisDataSet]
kNNSamplesByCategory k categories dataInput dataSet = [ categorySamples c | c <- categories ]
    where
        categorySamples c = [ dataSet !! i | i <- nnIndexes, c == (snd $ dataSet !! i) ]
        nnIndexes = kNearestNeighboursIndexes dataInput dataSet k

{-
Returns the predicted class for the given input using the kNN model.
# Input
k :: Int (Number of nearest neighbours to be considered)
categories :: [IrisCategory] (List of categories of the data set)
dataInput :: IrisDataInput (Vector to be used as reference)
dataSet :: IrisDataSet (List of vectors to have the distance measured)
# Ouput
category :: IrisCategory (Predicted category)
-}
kNNCategory :: Int -> [IrisCategory] -> IrisDataInput -> IrisDataSet -> IrisCategory
kNNCategory k categories dataInput dataSet = category
    where
        category = snd $ possibleNearestNeighbours !! nearestNeighbourIndex
        nearestNeighbourIndex = head $ kNearestNeighboursIndexes dataInput possibleNearestNeighbours 1
        possibleNearestNeighbours = foldl (++) [] [
                             samples | samples <- samplesByCategory,
                             (length samples) == (maximum categoriesOccurrences)
                            ]
        categoriesOccurrences = [ length categorySamples | categorySamples <- samplesByCategory ]
        samplesByCategory = kNNSamplesByCategory k categories dataInput dataSet
        
{-
Returns a list of the predicted classes for the test data set.
# Input
k :: Int (Number of nearest neighbours to be considered)
categories :: [IrisCategory] (List of categories of the data set)
testDataSet :: IrisDataSet (Test data set)
trainDataSet :: IrisDataSet (Train data set)
# Ouput
predictions :: [IrisCategory] (List of predictions using the train data set as "model"
                               and the test data set as data input.)
-}
predictDataSetNNeighbour :: Int -> [IrisCategory] -> IrisDataSet -> IrisDataSet -> [IrisCategory]
predictDataSetNNeighbour k categories testDataSet trainDataSet = nnCategories
    where
        nnCategories = [ kNNCategory k categories inputs trainDataSet | (inputs, _) <- testDataSet ]
