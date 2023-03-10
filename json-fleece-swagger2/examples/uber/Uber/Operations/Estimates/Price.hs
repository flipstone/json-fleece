{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Price
  ( operation
  , route
  , QueryParams(..)
  , queryParamsSchema
  , Responses(..)
  , responseSchemas
  ) where

import Beeline.HTTP.Client ((?+))
import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show, fmap)
import qualified Uber.Operations.Estimates.Price.EndLatitude as EndLatitude
import qualified Uber.Operations.Estimates.Price.EndLongitude as EndLongitude
import qualified Uber.Operations.Estimates.Price.StartLatitude as StartLatitude
import qualified Uber.Operations.Estimates.Price.StartLongitude as StartLongitude
import qualified Uber.Types.Error as Error
import qualified Uber.Types.PriceEstimate as PriceEstimate

operation ::
  H.Operation
    H.ContentTypeDecodingError
    H.NoPathParams
    QueryParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestQuerySchema = queryParamsSchema
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.get $
    R.make H.NoPathParams
      /- "estimates"
      /- "price"

data QueryParams = QueryParams
  { endLatitude :: EndLatitude.EndLatitude
  , endLongitude :: EndLongitude.EndLongitude
  , startLatitude :: StartLatitude.StartLatitude
  , startLongitude :: StartLongitude.StartLongitude
  }
  deriving (Eq, Show)

queryParamsSchema :: H.QuerySchema q => q QueryParams QueryParams
queryParamsSchema =
  H.makeQuery QueryParams
    ?+ H.required endLatitude EndLatitude.paramDef
    ?+ H.required endLongitude EndLongitude.paramDef
    ?+ H.required startLatitude StartLatitude.paramDef
    ?+ H.required startLongitude StartLongitude.paramDef

data Responses
  = Response200 [PriceEstimate.PriceEstimate]
  | OtherResponse Error.Error
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON (FC.list PriceEstimate.priceEstimateSchema)))
  , (H.AnyStatus, fmap OtherResponse (H.responseBody FA.JSON Error.errorSchema))
  ]