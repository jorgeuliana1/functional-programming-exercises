module Model.Centroid where
import DataSet.Types
import DataSet.Categories
import DataSet.Split
import Math.Vector

{-
Returns a vector (centroid of the given category).
# Input
dataSet :: DataSet (The data set to be fit)
category :: Category (The category to be fit)
# Ouput
centroid :: DataInput (Centroid of the given category)
-}
categoryCentroid :: DataSet -> Category -> DataInput
categoryCentroid dataSet category =
    sumVectors [
        inputs //// categoryLen
        | (inputs, valueCategory) <- dataSet, valueCategory == category
    ]
    where
        categoryLen = read (show $ categoryLength dataSet category) :: Double

{-
Returns a centroid for each given category.
# Input
dataSet :: DataSet (The data set to be fit)
categories :: [Category] (A list of the categories to be fit)
# Ouput
centroids :: [DataInput] (Centroids of the given category)
-}
centroids :: DataSet -> [Category] -> [DataInput]
centroids dataSet categories = [ categoryCentroid dataSet category | category <- categories ]

{-
Returns the index of the nearest centroid, given a list of centroids and a coordinate.
# Input
centroids :: [DataInput] (List of centroids)
dataInput :: DataInput (Informations of the data to be analyzed)
# Ouput
index :: Int (Index of the most probable class to fit in the given data input)
-}
nearestCentroidIndex :: [DataInput] -> DataInput -> Int
nearestCentroidIndex centroids dataInput = head $ take 1 mostProbableIndexes
    where
        mostProbableIndexes = [ i | i <- [0..], (distances !! i) == smallestCentroidDistance ]
        smallestCentroidDistance = minimum distances
        distances = [vectorsEuclideanDistance dataInput centroid | centroid <- centroids]

{-
Returns the predictions using the centroid algorithm, given the centroids coordinates, the default categories list
the test data set inputs.
# Input
centroids :: [DataInput] (List of centroids)
categories :: [Category] (List of categories.
                              Observation: The categories must have the same index as the
                              corresponding centroid in `centroids`.)
testDataSetInputs :: [DataInput] (The inputs to be evaluated by the model)
# Ouput
predictionOutputs :: [Category] (Predicted outputs, each index is correspondent to
                                     the same 'testDataSetInputs' index)
-}
predictDataSetCentroid :: [DataInput] -> [Category] -> [DataInput] -> [Category]
predictDataSetCentroid centroids categories testSetInputs = [ categories !! (nearestCentroidIndex centroids dInput)
                                                            | dInput <- testSetInputs ]

{-
Trains and predicts for the centroid method.
# Input
categories :: [Category] (Categories of the data set)
testDataSet :: DataSet (The data set for predictions)
trainDataSet :: DataSet (The data set for training)
# Ouput
predictionOutputs :: [Category] (Predicted outputs, each index is correspondent to
                                     the same 'testDataSetInputs' index)
-}
trainAndPredictDataSetCentroid :: [Category] -> DataSet -> DataSet -> [Category]
trainAndPredictDataSetCentroid categories testDataSet trainDataSet =
    predictDataSetCentroid (centroids trainDataSet categories) categories testSetInputs
    where
        (testSetInputs, testSetOutputs) = splitDataSetInputOutput testDataSet