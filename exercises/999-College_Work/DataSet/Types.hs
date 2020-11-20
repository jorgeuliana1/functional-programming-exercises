module DataSet.Types where
import Math.Vector

-- This module defines useful types for the DataSet manipulation.

type IrisCategory = String
type IrisDataInput = Vector Double
type IrisDataSet = [(Vector Double, IrisCategory)]
