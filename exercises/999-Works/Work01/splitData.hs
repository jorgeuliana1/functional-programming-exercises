module SplitData where
import System.Random

initializeRandomSettings :: Int -> IO ()
initializeRandomSettings seed = setStdGen (mkStdGen seed)

generateN :: Int -> [Int] -> IO Int
generateN maxValue ns = do
    n <- randomRIO (0, maxValue - 1) :: IO Int
    if (elem n ns) then
        generateN maxValue ns
        else return n

getRandomListWithoutRepetitions :: Int -> Int -> [Int] -> IO [Int]
getRandomListWithoutRepetitions 1 maxValue ns = do
    n <- generateN maxValue ns
    return (n:ns)
getRandomListWithoutRepetitions setLength maxValue ns = do
    n <- generateN maxValue ns
    ns <- getRandomListWithoutRepetitions (setLength - 1) maxValue (n:ns)
    return ns

getTestSetIndexes :: Int -> Float -> IO [Int]
getTestSetIndexes dataSetLen testSetPercentage = do
    let setLengthFlt = (read (show dataSetLen) :: Float) * testSetPercentage
    let setLengthInt = floor setLengthFlt
    ns <- getRandomListWithoutRepetitions setLengthInt dataSetLen []
    return ns