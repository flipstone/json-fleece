{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Products
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
import qualified Uber.Operations.Products.Latitude as Latitude
import qualified Uber.Operations.Products.Longitude as Longitude
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
      /- "products"

data QueryParams = QueryParams
  { latitude :: Latitude.Latitude
  , longitude :: Longitude.Longitude
  }
  deriving (Eq, Show)

queryParamsSchema :: P.QuerySchema p => p QueryParams QueryParams
queryParamsSchema =
  P.makeParams QueryParams
    ?+ P.required latitude Latitude.paramDef
    ?+ P.required longitude Longitude.paramDef

data Responses
  = Response200 [Product.Product]
  | OtherResponse Error.Error
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON (FC.list Product.productSchema)))
  , (H.AnyStatus, fmap OtherResponse (H.responseBody FA.JSON Error.errorSchema))
  ]