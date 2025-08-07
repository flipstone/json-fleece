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

import qualified Beeline.HTTP.Client as H
import Beeline.Params ((?+))
import qualified Beeline.Params as P
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
    FA.JSONDecodingError
    H.NoPathParams
    QueryParams
    H.NoHeaderParams
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

queryParamsSchema :: P.QuerySchema p => p QueryParams QueryParams
queryParamsSchema =
  P.makeParams QueryParams
    ?+ P.required endLatitude EndLatitude.paramDef
    ?+ P.required endLongitude EndLongitude.paramDef
    ?+ P.required startLatitude StartLatitude.paramDef
    ?+ P.required startLongitude StartLongitude.paramDef

data Responses
  = Response200 [PriceEstimate.PriceEstimate]
  | OtherResponse Error.Error
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON (FC.list PriceEstimate.priceEstimateSchema)))
  , (H.AnyStatus, fmap OtherResponse (H.responseBody FA.JSON Error.errorSchema))
  ]