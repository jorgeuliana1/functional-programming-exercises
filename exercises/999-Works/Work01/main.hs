import ReadCSV
import SplitData
import System.Random

main = do
    initializeRandomSettings 20
    n <- getTestSetIndexes 10 0.5
    print n