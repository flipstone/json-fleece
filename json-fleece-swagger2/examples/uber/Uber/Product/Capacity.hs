{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Product.Capacity
  ( Capacity(..)
  , capacitySchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Capacity = Capacity T.Text
  deriving (Show, Eq)

capacitySchema :: FC.Fleece schema => schema Capacity
capacitySchema =
  FC.coerceSchema FC.text