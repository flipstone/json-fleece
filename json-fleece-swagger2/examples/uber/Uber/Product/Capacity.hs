{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Product.Capacity
  ( Capacity(..)
  , capacitySchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Capacity = Capacity Text
  deriving (Show, Eq)

capacitySchema :: FC.Fleece schema => schema Capacity
capacitySchema =
  FC.coerceSchema FC.text