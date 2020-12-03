module DataSet.Types where
import Math.Vector

-- This module defines useful types for the DataSet manipulation.

type Category = String
type DataInput = Vector Double
type DataSet = [(Vector Double, Category)]
