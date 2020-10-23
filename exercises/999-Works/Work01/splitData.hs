module SplitData where
import System.Random
import DataSet

-- This function simplifies the proccess of initialization of random generator.
initializeRandomSettings :: Int -> IO ()
initializeRandomSettings seed = setStdGen (mkStdGen seed)

-- This function garantees that no number in the generated list will be presented twice.
generateN :: Int -> [Int] -> IO Int
generateN maxValue ns = do
    n <- randomRIO (0, maxValue - 1) :: IO Int
    if (elem n ns) then
        generateN maxValue ns
        else return n

-- The name is self-explanatory.
getRandomListWithoutRepetitions :: Int -> Int -> [Int] -> IO [Int]
getRandomListWithoutRepetitions 1 maxValue ns = do -- ns must start as an empty list.
    n <- generateN maxValue ns
    return (n:ns)
getRandomListWithoutRepetitions setLength maxValue ns = do
    n <- generateN maxValue ns
    ns <- getRandomListWithoutRepetitions (setLength - 1) maxValue (n:ns)
    return ns

{- An elegant wrapper for the getRandomListWithoutRepetitions function.
   This function is called when you want to generate a list of random integers. -}
getTestSetIndexes :: Int -> Float -> IO [Int]
getTestSetIndexes dataSetLen testSetPercentage = do
    let setLengthFlt = (read (show dataSetLen) :: Float) * testSetPercentage
    let setLengthInt = floor setLengthFlt
    ns <- getRandomListWithoutRepetitions setLengthInt dataSetLen []
    return ns

{- This function receives a dataset and the output of the getTestSetIndexes function and returns the
   correspondent splitted datasets. -}
splitDataSet :: IrisDataSet -> [Int] -> (IrisDataSet, IrisDataSet)
splitDataSet dataSet testSetIndexes = 
    ([dataSet !! i | i <- [0..(length dataSet) - 1], not (elem i testSetIndexes)], -- Getting the train set
     [dataSet !! i | i <- testSetIndexes]) -- Getting the test set