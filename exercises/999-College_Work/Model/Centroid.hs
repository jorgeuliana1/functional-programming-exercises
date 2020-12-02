module Model.Centroid where
import DataSet.Types
import DataSet.Categories
import DataSet.Split
import Math.Vector

{-
Returns a vector (centroid of the given category).
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
        categoryLen = read (show $ categoryLength dataSet category) :: Double

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
nearestCentroidIndex centroids dataInput = head $ take 1 mostProbableIndexes
    where
        mostProbableIndexes = [ i | i <- [0..], (distances !! i) == smallestCentroidDistance ]
        smallestCentroidDistance = minimum distances
        distances = [vectorsEuclideanDistance dataInput centroid | centroid <- centroids]

{-
Returns the predictions using the centroid algorithm, given the centroids coordinates, the default categories list
the test data set inputs.
# Input
centroids :: [IrisDataInput] (List of centroids)
categories :: [IrisCategory] (List of categories.
                              Observation: The categories must have the same index as the
                              corresponding centroid in `centroids`.)
testDataSetInputs :: [IrisDataInput] (The inputs to be evaluated by the model)
# Ouput
predictionOutputs :: [IrisCategory] (Predicted outputs, each index is correspondent to
                                     the same 'testDataSetInputs' index)
-}
predictDataSetCentroid :: [IrisDataInput] -> [IrisCategory] -> [IrisDataInput] -> [IrisCategory]
predictDataSetCentroid centroids categories testSetInputs = [ categories !! (nearestCentroidIndex centroids dInput)
                                                            | dInput <- testSetInputs ]

{-
Trains and predicts for the centroid method.
# Input
categories :: [IrisCategory] (Categories of the data set)
testDataSet :: IrisDataSet (The data set for predictions)
trainDataSet :: IrisDataSet (The data set for training)
# Ouput
predictionOutputs :: [IrisCategory] (Predicted outputs, each index is correspondent to
                                     the same 'testDataSetInputs' index)
-}
trainAndPredictDataSetCentroid :: [IrisCategory] -> IrisDataSet -> IrisDataSet -> [IrisCategory]
trainAndPredictDataSetCentroid categories testDataSet trainDataSet =
    predictDataSetCentroid (centroids trainDataSet categories) categories testSetInputs
    where
        (testSetInputs, testSetOutputs) = splitDataSetInputOutput testDataSet