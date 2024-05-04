{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Time
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
import Prelude (($), Eq, Maybe, Show, fmap)
import qualified Uber.Operations.Estimates.Time.CustomerUuid as CustomerUuid
import qualified Uber.Operations.Estimates.Time.ProductId as ProductId
import qualified Uber.Operations.Estimates.Time.StartLatitude as StartLatitude
import qualified Uber.Operations.Estimates.Time.StartLongitude as StartLongitude
import qualified Uber.Types.Error as Error
import qualified Uber.Types.Product as Product

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
      /- "time"

data QueryParams = QueryParams
  { customerUuid :: Maybe CustomerUuid.CustomerUuid
  , productId :: Maybe ProductId.ProductId
  , startLatitude :: StartLatitude.StartLatitude
  , startLongitude :: StartLongitude.StartLongitude
  }
  deriving (Eq, Show)

queryParamsSchema :: H.ParameterCollectionSchema p => p QueryParams QueryParams
queryParamsSchema =
  H.makeParams QueryParams
    ?+ H.optional customerUuid CustomerUuid.paramDef
    ?+ H.optional productId ProductId.paramDef
    ?+ H.required startLatitude StartLatitude.paramDef
    ?+ H.required startLongitude StartLongitude.paramDef

data Responses
  = Response200 [Product.Product]
  | OtherResponse Error.Error
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON (FC.list Product.productSchema)))
  , (H.AnyStatus, fmap OtherResponse (H.responseBody FA.JSON Error.errorSchema))
  ]