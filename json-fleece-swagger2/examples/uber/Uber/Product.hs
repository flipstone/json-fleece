{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Product
  ( Product(..)
  , productSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import Uber.Product.Capacity (Capacity, capacitySchema)
import Uber.Product.Description (Description, descriptionSchema)
import Uber.Product.DisplayName (DisplayName, displayNameSchema)
import Uber.Product.Image (Image, imageSchema)
import Uber.Product.ProductId (ProductId, productIdSchema)

data Product = Product
  { displayName :: Maybe DisplayName -- ^ Display name of product.
  , description :: Maybe Description -- ^ Description of product.
  , productId :: Maybe ProductId -- ^ Unique identifier representing a specific product for a given latitude & longitude. For example, uberX in San Francisco will have a different product_id than uberX in Los Angeles.
  , image :: Maybe Image -- ^ Image URL representing the product.
  , capacity :: Maybe Capacity -- ^ Capacity of product. For example, 4 people.
  }
  deriving (Eq, Show)

productSchema :: FC.Fleece schema => schema Product
productSchema =
  FC.object $
    FC.constructor Product
      #+ FC.optional "display_name" displayName displayNameSchema
      #+ FC.optional "description" description descriptionSchema
      #+ FC.optional "product_id" productId productIdSchema
      #+ FC.optional "image" image imageSchema
      #+ FC.optional "capacity" capacity capacitySchema