{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.ProductAlias
  ( ProductAlias
  , productAliasSchema
  ) where

import qualified Fleece.Core as FC
import qualified Uber.Types.Product as Product

type ProductAlias = Product.Product

productAliasSchema :: FC.Fleece t => FC.Schema t ProductAlias
productAliasSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "Uber.Types.ProductAlias" "ProductAlias")
    Product.productSchema