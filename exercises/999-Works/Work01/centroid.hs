module Centroid where
import DataSet
import TupleMath

-- Returns a quadruple of centroids of the given category.
categoryCentroid :: IrisDataSet -> IrisCategory -> IrisDataInput
categoryCentroid dataSet category =
    sumQuadruples [
        ( value1, value2, value3, value4 ) //// categoryLen
        | (value1, value2, value3, value4, valueCategory) <- dataSet, valueCategory == category
    ]
    where
        categoryLen = read (show (categoryLengthForIrisDataSet dataSet category)) :: Float

-- Returns a centroid for each given category.
centroids :: IrisDataSet -> [IrisCategory] -> [IrisDataInput]
centroids dataSet categories = [ categoryCentroid dataSet category | category <- categories ]
