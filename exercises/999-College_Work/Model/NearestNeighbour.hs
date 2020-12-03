module Model.NearestNeighbour where
import Math.Vector
import DataSet.Types
import DataSet.Categories
import Algorithm.Sorting

{-
Gets a list of the distance of the given vector (vector) to a list of vectors (vector) in R4.
# Input
dataInput :: DataInput (Vector to be used as reference)
dataSet :: DataSet (List of vectors to have the distance measured)
# Ouput
distances :: [Float] (Distances from dataInput to the vectors of dataSet.
                      each index in `distances` is correspondent to the
                      same index in `dataSet`)
-}
neighboursDistance :: DataInput -> DataSet -> [Double]
neighboursDistance dataInput dataSet = [ vectorsEuclideanDistance dataInput inputs | (inputs, _) <- dataSet ]

{-
Returns the indexes of the k (given) nearest neighbours.
# Input
dataInput :: DataInput (Vector to be used as reference)
dataSet :: DataSet (List of vector to have the distance measured)
k :: Int (The number of indexes to be returned)
# Output
indexes :: [Int] (The k nearest neighbours indexes in `dataSet`)
-}
kNearestNeighboursIndexes :: DataInput -> DataSet -> Int -> [Int]
kNearestNeighboursIndexes dataInput dataSet k =
    take k probableIndexes
    where
        probableIndexes = foldr (++) [] indexesByDistance
        indexesByDistance = [ indexesWithTheDistance d | d <- cresDistances ]
        indexesWithTheDistance d = filter (\i -> (currIndexDistance i) == d) [0..(length dataSet) - 1]
        currIndexDistance i = vectorsEuclideanDistance dataInput $ currIndexVector i
        currIndexVector i = fst $ dataSet !! i
        cresDistances {- The k neighbours distances in crescent order -} =
            take k $ quickSort $ neighboursDistance dataInput dataSet

{-
Returns the index of the nearest neighbour.
# Input
k :: Int (Number of nearest neighbours to be considered)
dataInput :: DataInput (Vector to be used as reference)
dataSet :: DataSet (List of vectors to have the distance measured)
# Ouput
index :: Int (Nearest neighbour index in `dataSet`)
-}
nearestNeighbourIndex :: Int -> DataInput -> DataSet -> Int
nearestNeighbourIndex k dataInput dataSet = head $ kNearestNeighboursIndexes dataInput dataSet k

{-
Returns the nearest neighbours splitted into data sets.
# Input
k :: Int (Number of nearest neighbours to be considered)
categories :: [Category] (List of categories of the data set)
dataInput :: DataInput (Vector to be used as reference)
dataSet :: DataSet (List of vectors to have the distance measured)
# Ouput
category :: [DataSet] (List of data sets organized by category)
-}
kNNSamplesByCategory :: Int -> [Category] -> DataInput -> DataSet -> [DataSet]
kNNSamplesByCategory k categories dataInput dataSet = [ categorySamples c | c <- categories ]
    where
        categorySamples c = [ dataSet !! i | i <- nnIndexes, c == (snd $ dataSet !! i) ]
        nnIndexes = kNearestNeighboursIndexes dataInput dataSet k

{-
Returns the predicted class for the given input using the kNN model.
# Input
k :: Int (Number of nearest neighbours to be considered)
categories :: [Category] (List of categories of the data set)
dataInput :: DataInput (Vector to be used as reference)
dataSet :: DataSet (List of vectors to have the distance measured)
# Ouput
category :: Category (Predicted category)
-}
kNNCategory :: Int -> [Category] -> DataInput -> DataSet -> Category
kNNCategory k categories dataInput dataSet = category
    where
        category = head [ c | (distance, c) <- probableCategories, distance == smallestDistance ]
        smallestDistance = minimum [ distance | (distance, _) <- probableCategories ]
        probableCategories = mostProbableCategories k categories dataInput dataSet
 
mostProbableCategories :: Int -> [Category] -> DataInput -> DataSet -> [(Double, Category)]
mostProbableCategories k categories dataInput dataSet = 
    [(categorySumOfDistances c, c) | c <- dataSetCategories possibleNearestNeighbours]
    where
        categorySumOfDistances c =
            sum [vectorsEuclideanDistance dataInput v | (v, cat) <- possibleNearestNeighbours, c == cat]
        possibleNearestNeighbours = foldr (++) [] [
                                samples | samples <- samplesByCategory,
                                (length samples) == (maximum categoriesOccurrences)
                                ]
        categoriesOccurrences = [ length categorySamples | categorySamples <- samplesByCategory ]
        samplesByCategory = kNNSamplesByCategory k categories dataInput dataSet

{-
Returns a list of the predicted classes for the test data set.
# Input
k :: Int (Number of nearest neighbours to be considered)
categories :: [Category] (List of categories of the data set)
testDataSet :: DataSet (Test data set)
trainDataSet :: DataSet (Train data set)
# Ouput
predictions :: [Category] (List of predictions using the train data set as "model"
                               and the test data set as data input.)
-}
predictDataSetNNeighbour :: Int -> [Category] -> DataSet -> DataSet -> [Category]
predictDataSetNNeighbour k categories testDataSet trainDataSet = nnCategories
    where
        nnCategories = [ kNNCategory k categories inputs trainDataSet | (inputs, _) <- testDataSet ]
