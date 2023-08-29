{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.PriceEstimate
  ( PriceEstimate(..)
  , priceEstimateSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified Uber.Types.PriceEstimate.CurrencyCode as CurrencyCode
import qualified Uber.Types.PriceEstimate.DisplayName as DisplayName
import qualified Uber.Types.PriceEstimate.Estimate as Estimate
import qualified Uber.Types.PriceEstimate.HighEstimate as HighEstimate
import qualified Uber.Types.PriceEstimate.LowEstimate as LowEstimate
import qualified Uber.Types.PriceEstimate.ProductId as ProductId
import qualified Uber.Types.PriceEstimate.SurgeMultiplier as SurgeMultiplier

data PriceEstimate = PriceEstimate
  { currencyCode :: Maybe CurrencyCode.CurrencyCode -- ^ [ISO 4217](http://en.wikipedia.org/wiki/ISO_4217) currency code.
  , productId :: Maybe ProductId.ProductId -- ^ Unique identifier representing a specific product for a given latitude & longitude. For example, uberX in San Francisco will have a different product_id than uberX in Los Angeles
  , displayName :: Maybe DisplayName.DisplayName -- ^ Display name of product.
  , surgeMultiplier :: Maybe SurgeMultiplier.SurgeMultiplier -- ^ Expected surge multiplier. Surge is active if surge_multiplier is greater than 1. Price estimate already factors in the surge multiplier.
  , highEstimate :: Maybe HighEstimate.HighEstimate -- ^ Upper bound of the estimated price.
  , lowEstimate :: Maybe LowEstimate.LowEstimate -- ^ Lower bound of the estimated price.
  , estimate :: Maybe Estimate.Estimate -- ^ Formatted string of estimate in local currency of the start location. Estimate could be a range, a single number (flat rate) or "Metered" for TAXI.
  }
  deriving (Eq, Show)

priceEstimateSchema :: FC.Fleece schema => schema PriceEstimate
priceEstimateSchema =
  FC.object $
    FC.constructor PriceEstimate
      #+ FC.optional "currency_code" currencyCode CurrencyCode.currencyCodeSchema
      #+ FC.optional "product_id" productId ProductId.productIdSchema
      #+ FC.optional "display_name" displayName DisplayName.displayNameSchema
      #+ FC.optional "surge_multiplier" surgeMultiplier SurgeMultiplier.surgeMultiplierSchema
      #+ FC.optional "high_estimate" highEstimate HighEstimate.highEstimateSchema
      #+ FC.optional "low_estimate" lowEstimate LowEstimate.lowEstimateSchema
      #+ FC.optional "estimate" estimate Estimate.estimateSchema