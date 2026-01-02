{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Product.ProductId
  ( ProductId(..)
  , productIdSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ProductId = ProductId T.Text
  deriving (Show, Eq)

productIdSchema :: FC.Fleece t => FC.Schema t ProductId
productIdSchema =
  FC.coerceSchema FC.text