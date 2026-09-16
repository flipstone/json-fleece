{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.ProductAlias
  ( ProductAlias(..)
  , productAliasSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified Uber.Types.Product as Product

newtype ProductAlias = ProductAlias Product.Product
  deriving (Show, Eq)

productAliasSchema :: FC.Fleece t => FC.Schema t ProductAlias
productAliasSchema =
  FC.coerceSchema Product.productSchema