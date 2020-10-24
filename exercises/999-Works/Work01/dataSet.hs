module DataSet where
type IrisCategory = String
type IrisDataInput = (Float, Float, Float, Float)
type IrisDataSet = [(Float, Float, Float, Float, IrisCategory)]

-- Returns the amount of samples of a certain category (adapted for Iris DataSet)
categoryLengthForIrisDataSet :: IrisDataSet -> IrisCategory -> Int
categoryLengthForIrisDataSet dataSet category = sum [ 1 | (_, _, _, _, samCat) <- dataSet, samCat == category ]

-- Returns a list of the categories contained in the given dataset
dataSetCategoriesList :: IrisDataSet -> Int -> [IrisCategory] -> [IrisCategory]
dataSetCategoriesList dataSet 1 categories = do
    let (_, _, _, _, currentCategory) = dataSet !! 0
    if (elem currentCategory categories)
        then categories
        else
            currentCategory:categories
dataSetCategoriesList dataSet currentIndex categories = do
    let (_, _, _, _, currentCategory) = dataSet !! (currentIndex - 1)
    if (elem currentCategory categories)
        then dataSetCategoriesList dataSet (currentIndex - 1) categories
        else
            dataSetCategoriesList dataSet (currentIndex - 1) (currentCategory:categories)

-- An elegant wrapper for dataSetCategoriesList.
dataSetCategories :: IrisDataSet -> [IrisCategory]
dataSetCategories dataSet = dataSetCategoriesList dataSet (length dataSet) []

-- Splits dataset in input and output.
splitDataSetInputOutput :: IrisDataSet -> ([IrisDataInput], [IrisCategory])
splitDataSetInputOutput dataSet = (input, output)
    where
        input = [ (a, b, c, d) | (a, b, c, d, _) <- dataSet ]
        output = [ e | (_, _, _, _, e) <- dataSet ]