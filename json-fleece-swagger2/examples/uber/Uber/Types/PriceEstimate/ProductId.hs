{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.PriceEstimate.ProductId
  ( ProductId(..)
  , productIdSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ProductId = ProductId T.Text
  deriving (Show, Eq)

productIdSchema :: FC.Fleece schema => schema ProductId
productIdSchema =
  FC.coerceSchema FC.text