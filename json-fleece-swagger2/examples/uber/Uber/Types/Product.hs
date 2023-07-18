{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Product
  ( Product(..)
  , productSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified Uber.Types.Product.Capacity as Capacity
import qualified Uber.Types.Product.Description as Description
import qualified Uber.Types.Product.DisplayName as DisplayName
import qualified Uber.Types.Product.Image as Image
import qualified Uber.Types.Product.ProductId as ProductId

data Product = Product
  { capacity :: Maybe Capacity.Capacity -- ^ Capacity of product. For example, 4 people.
  , productId :: Maybe ProductId.ProductId -- ^ Unique identifier representing a specific product for a given latitude & longitude. For example, uberX in San Francisco will have a different product_id than uberX in Los Angeles.
  , image :: Maybe Image.Image -- ^ Image URL representing the product.
  , description :: Maybe Description.Description -- ^ Description of product.
  , displayName :: Maybe DisplayName.DisplayName -- ^ Display name of product.
  }
  deriving (Eq, Show)

productSchema :: FC.Fleece schema => schema Product
productSchema =
  FC.object $
    FC.constructor Product
      #+ FC.optional "capacity" capacity Capacity.capacitySchema
      #+ FC.optional "product_id" productId ProductId.productIdSchema
      #+ FC.optional "image" image Image.imageSchema
      #+ FC.optional "description" description Description.descriptionSchema
      #+ FC.optional "display_name" displayName DisplayName.displayNameSchema