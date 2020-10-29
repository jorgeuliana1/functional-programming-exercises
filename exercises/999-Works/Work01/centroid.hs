module Centroid where
import DataSet
import DataSetCategories
import TupleMath

{-
Returns a quadruple (centroid of the given category).
# Input
dataSet :: IrisDataSet (The data set to be fit)
category :: IrisCategory (The category to be fit)
# Ouput
centroid :: IrisDataInput (Centroid of the given category)
-}
categoryCentroid :: IrisDataSet -> IrisCategory -> IrisDataInput
categoryCentroid dataSet category =
    sumVectors [
        inputs //// categoryLen
        | (inputs, valueCategory) <- dataSet, valueCategory == category
    ]
    where
        categoryLen = read (show (categoryLength dataSet category)) :: Double

{-
Returns a centroid for each given category.
# Input
dataSet :: IrisDataSet (The data set to be fit)
categories :: [IrisCategory] (A list of the categories to be fit)
# Ouput
centroids :: [IrisDataInput] (Centroids of the given category)
-}
centroids :: IrisDataSet -> [IrisCategory] -> [IrisDataInput]
centroids dataSet categories = [ categoryCentroid dataSet category | category <- categories ]

{-
Returns the index of the nearest centroid, given a list of centroids and a coordinate.
# Input
centroids :: [IrisDataInput] (List of centroids)
dataInput :: IrisDataInput (Informations of the data to be analyzed)
# Ouput
index :: Int (Index of the most probable class to fit in the given data input)
-}
nearestCentroidIndex :: [IrisDataInput] -> IrisDataInput -> Int
nearestCentroidIndex centroids dataInput = head (take 1 [ i | i <- [0..], (distances !! i) == smallestCentroidDistance ])
    where
        smallestCentroidDistance = minimum distances
        distances = [euclideanDistance dataInput centroid | centroid <- centroids]

{-
Returns the accuracy of the centroid algorithm, given the centroids coordinates, the default categories list
the test dataset inputs and outputs.
# Input
centroids :: [IrisDataInput] (List of centroids)
categories :: [IrisCategory] (List of categories.
                              Observation: The categories must have the same index as the
                              corresponding centroid in `centroids`.)
testDataSetInputs :: [IrisDataInput] (The inputs to be evaluated by the model)
testDataSetOutputs :: [IrisCategory] (Expected outputs, each index is correspondent to
                                      the same 'testDataSetInputs' index)
# Ouput
accurary :: Float (Floating point number in the interval [0, 1] representing the percentage
                   of correct predictions of the model.)
-}
centroidAccuracy :: [IrisDataInput] -> [IrisCategory] -> [IrisDataInput] -> [IrisCategory] -> Double
centroidAccuracy centroids categories testDataSetInputs testDataSetOutputs = sum resultsList
    where
        resultsList = [ 1.00 / testDataSetLength
            |i <- [0..((length testDataSetOutputs) - 1)],
            (categories !! (centroidIndex i)) ==  (testDataSetOutputs !! i)]
        testDataSetLength = read (show (length testDataSetInputs)) :: Double
        centroidIndex i = nearestCentroidIndex centroids (testDataSetInputs !! i)