{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate
  ( PriceEstimate(..)
  , priceEstimateSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import Uber.PriceEstimate.CurrencyCode (CurrencyCode, currencyCodeSchema)
import Uber.PriceEstimate.DisplayName (DisplayName, displayNameSchema)
import Uber.PriceEstimate.Estimate (Estimate, estimateSchema)
import Uber.PriceEstimate.HighEstimate (HighEstimate, highEstimateSchema)
import Uber.PriceEstimate.LowEstimate (LowEstimate, lowEstimateSchema)
import Uber.PriceEstimate.ProductId (ProductId, productIdSchema)
import Uber.PriceEstimate.SurgeMultiplier (SurgeMultiplier, surgeMultiplierSchema)

data PriceEstimate = PriceEstimate
  { displayName :: Maybe DisplayName -- ^ Display name of product.
  , currencyCode :: Maybe CurrencyCode -- ^ [ISO 4217](http://en.wikipedia.org/wiki/ISO_4217) currency code.
  , estimate :: Maybe Estimate -- ^ Formatted string of estimate in local currency of the start location. Estimate could be a range, a single number (flat rate) or "Metered" for TAXI.
  , productId :: Maybe ProductId -- ^ Unique identifier representing a specific product for a given latitude & longitude. For example, uberX in San Francisco will have a different product_id than uberX in Los Angeles
  , highEstimate :: Maybe HighEstimate -- ^ Upper bound of the estimated price.
  , lowEstimate :: Maybe LowEstimate -- ^ Lower bound of the estimated price.
  , surgeMultiplier :: Maybe SurgeMultiplier -- ^ Expected surge multiplier. Surge is active if surge_multiplier is greater than 1. Price estimate already factors in the surge multiplier.
  }
  deriving (Eq, Show)

priceEstimateSchema :: FC.Fleece schema => schema PriceEstimate
priceEstimateSchema =
  FC.object $
    FC.constructor PriceEstimate
      #+ FC.optional "display_name" displayName displayNameSchema
      #+ FC.optional "currency_code" currencyCode currencyCodeSchema
      #+ FC.optional "estimate" estimate estimateSchema
      #+ FC.optional "product_id" productId productIdSchema
      #+ FC.optional "high_estimate" highEstimate highEstimateSchema
      #+ FC.optional "low_estimate" lowEstimate lowEstimateSchema
      #+ FC.optional "surge_multiplier" surgeMultiplier surgeMultiplierSchema