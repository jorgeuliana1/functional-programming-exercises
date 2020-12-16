module DataSet.Split where
import System.Random
import DataSet.Types

-- This function simplifies the proccess of initialization of random number generator.
initializeRandomSettings :: Int -> IO ()
initializeRandomSettings seed = setStdGen (mkStdGen seed)

{-
Generates an unique number N.
# Input
maxValue :: Int (Biggest number to be generated, the smallest is 0)
ns :: [Int] (List of preexisting numbers of the list, these numbers won't be
             generated anymore)
# Output
n :: IO Int (Random unique number)
-}
generateN :: Int -> [Int] -> IO Int
generateN maxValue ns = do
    n <- randomRIO (0, maxValue - 1) :: IO Int
    if (elem n ns) then
        generateN maxValue ns
        else return n

{-
Generates a list of numbers without any repetition.
# Input
setLength :: Int (Number of elements to be generated)
maxValue :: Int (Biggest value to be generated)
ns :: [Int] (Initial values of the list, in doubt, consider as [])
-}
getRandomListWithoutRepetitions :: Int -> Int -> [Int] -> IO [Int]
getRandomListWithoutRepetitions 1 maxValue ns = do -- ns must start as an empty list.
    n <- generateN maxValue ns
    return (n:ns)
getRandomListWithoutRepetitions setLength maxValue ns = do
    n <- generateN maxValue ns
    ns <- getRandomListWithoutRepetitions (setLength - 1) maxValue (n:ns)
    return $ reverse ns

{-
Gemerates a list of random indexes of test dataset without any repetition.
# Input
dataSetLen :: Int (Length of the data set (including test and train sets))
testSetPercentage :: Float (Percentage of the data set that will be part of the test data set
                            Goes from 0 to 1)
# Output
ns :: IO [Int] (List of the indexes to be considered the test set indexes)
-}
getTestSetIndexes :: Int -> Float -> IO [Int]
getTestSetIndexes dataSetLen testSetPercentage = do
    let setLengthFlt = (read (show dataSetLen) :: Float) * testSetPercentage
    let setLengthInt = floor setLengthFlt
    ns <- getRandomListWithoutRepetitions setLengthInt dataSetLen []
    return ns

{-
Splits the data set in train set and test set.
# Input
dataSet :: DataSet (Original full-length data set)
testSetIndexes :: [Int] (Indexes to be transferred to the test set)
# Output
trainTestSets :: (DataSet, DataSet) (Tuple of data sets, the first is the train set
                                             and the second is the test set)
-}
splitDataSet :: DataSet -> [Int] -> (DataSet, DataSet)
splitDataSet dataSet testSetIndexes = 
    ([dataSet !! i | i <- [0..(length dataSet) - 1], not (elem i testSetIndexes)], -- Getting the train set
     [dataSet !! i | i <- testSetIndexes]) -- Getting the test set

{-
Splits dataset in input and output.
# Input
dataSet :: DataSet (Data set to be splitted into input and output)
# Output
inputOutput :: ([DataInput], [Category]) (Tuple containing the data
                                                  set input and the correspondent
                                                  expected output)
-}
splitDataSetInputOutput :: DataSet -> ([DataInput], [Category])
splitDataSetInputOutput dataSet = (input, output)
    where
        input = [ inputs | (inputs, _) <- dataSet ]
        output = [ category | (_, category) <- dataSet ]
