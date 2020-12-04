module DataSet.Standardization (standardizeDataSet) where
import Math.Vector
import Math.Matrix
import DataSet.Types
{--
Converts the data set to a matrix, making it easier to work with some mathematical resources.
# Input:
dataSet :: DataSet (The data set to be converted)
# Output:
matrix :: Matrix Double (The resulting matrix)
--}
dataSetToMatrix :: DataSet -> Matrix Double
dataSetToMatrix dataSet = Matrix [ dataInput | (dataInput, _) <- dataSet ]

{--
Converts the matrix to a data set, making it easier to work with statistical models.
# Input:
dataSet :: DataSet (The data set for categories reference)
m :: Matrix Double (The matrix to be converted to data set)
# Output:
dataSet :: DataSet (The resulting data set)
--}
matrixToDataSet :: DataSet -> Matrix Double -> DataSet
matrixToDataSet dataSet (Matrix m) = zip [ dataInput | dataInput <- m ] [ category | (_, category) <- dataSet]

{--
Standardizes the matrix.
# Input:
dataSet :: DataSet (The data set to be standardized)
baseDataSet :: DataSet
# Output:
dataSet :: DataSet (The standardized data set)
--}
standardizeDataSet :: DataSet -> DataSet -> DataSet
standardizeDataSet dataSet baseDataSet =
    matrixToDataSet dataSet $ matrixStandardizeWithBase (dataSetToMatrix baseDataSet) (dataSetToMatrix dataSet)