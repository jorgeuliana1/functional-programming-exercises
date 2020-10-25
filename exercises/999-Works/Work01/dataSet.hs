module DataSet where

-- This module defines useful types for the DataSet manipulation.

type IrisCategory = String
type IrisDataInput = (Float, Float, Float, Float)
type IrisDataSet = [(Float, Float, Float, Float, IrisCategory)]