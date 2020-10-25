module Centroid where
import DataSet
import DataSetCategories
import TupleMath

-- Returns a quadruple of centroids of the given category.
categoryCentroid :: IrisDataSet -> IrisCategory -> IrisDataInput
categoryCentroid dataSet category =
    sumQuadruples [
        ( value1, value2, value3, value4 ) //// categoryLen
        | (value1, value2, value3, value4, valueCategory) <- dataSet, valueCategory == category
    ]
    where
        categoryLen = read (show (categoryLength dataSet category)) :: Float

-- Returns a centroid for each given category.
centroids :: IrisDataSet -> [IrisCategory] -> [IrisDataInput]
centroids dataSet categories = [ categoryCentroid dataSet category | category <- categories ]

-- Returns the index of the nearest centroid, given a list of centroids and a coordinate.
nearestCentroidIndex :: [IrisDataInput] -> IrisDataInput -> Int
nearestCentroidIndex centroids dataInput = head (take 1 [ i | i <- [0..], (distances !! i) == smallestCentroidDistance ])
    where
        smallestCentroidDistance = tupleMinimum distances
        distances = [distance dataInput centroid | centroid <- centroids]
        distance quadrupleA quadrupleB = tupleSqrt (tupleAbs ((quadrupleA ^^^^ 2) `tupleSubtraction` (quadrupleB ^^^^ 2)))
        -- Euclidian distance

{- Returns the accuracy of the centroid algorithm, given the centroids coordinates, the default categories list
   the test dataset inputs and outputs. -}
centroidAccuracy :: [IrisDataInput] -> [IrisCategory] -> [IrisDataInput] -> [IrisCategory] -> Float
centroidAccuracy centroids categories testDataSetInputs testDataSetOutputs = sum resultsList
    where
        resultsList = [ 1.00 / testDataSetLength
            |i <- [0..((length testDataSetOutputs) - 1)],
            (categories !! (centroidIndex i)) ==  (testDataSetOutputs !! i)]
        testDataSetLength = read (show (length testDataSetInputs)) :: Float
        centroidIndex i = nearestCentroidIndex centroids (testDataSetInputs !! i)