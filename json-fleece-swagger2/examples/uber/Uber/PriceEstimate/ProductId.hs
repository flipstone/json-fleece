{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate.ProductId
  ( ProductId(..)
  , productIdSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ProductId = ProductId Text
  deriving (Show, Eq)

productIdSchema :: FC.Fleece schema => schema ProductId
productIdSchema =
  FC.coerceSchema FC.text