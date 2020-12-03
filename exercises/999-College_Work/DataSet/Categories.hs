module DataSet.Categories where
import DataSet.Types

{-
Returns the amount of samples of a certain category.
# Input
dataSet :: DataSet (The data set to be analyzed)
category :: Category (The category to be analyzed)
# Output
length :: Int (The number of elements of a certain category in the data set)
-}
categoryLength :: DataSet -> Category -> Int
categoryLength dataSet category = sum [ 1 | (_, samCat) <- dataSet, samCat == category ]

{-
Returns a list of the categories contained in the given data set (no repetitions).
# Input
dataSet :: DataSet (The data set to be analyzed)
currentIndex :: Int (The current index of the "iteration"
                    (Start with the length of the data set))
categories :: [Category] (List of categories found
                              until the current "iteration")
# Output
categories :: [Category] (List of the categories contained in the data set)
-}
dataSetCategoriesList :: DataSet -> Int -> [Category] -> [Category]
dataSetCategoriesList dataSet 1 categories = do
    let (_, currentCategory) = dataSet !! 0
    if (elem currentCategory categories)
        then categories
        else
            currentCategory:categories
dataSetCategoriesList dataSet currentIndex categories = do
    let (_, currentCategory) = dataSet !! (currentIndex - 1)
    if (elem currentCategory categories)
        then dataSetCategoriesList dataSet (currentIndex - 1) categories
        else
            dataSetCategoriesList dataSet (currentIndex - 1) (currentCategory:categories)

{-
An elegant wrapper for dataSetCategoriesList.
# Input
dataSet :: DataSet (The data set to be analyzed)
# Ouput
categories :: [Category] (The categories of the data set)
-}
dataSetCategories :: DataSet -> [Category]
dataSetCategories dataSet = dataSetCategoriesList dataSet (length dataSet) []
