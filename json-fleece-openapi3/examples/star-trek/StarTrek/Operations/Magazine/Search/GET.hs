{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Magazine.Search.GET
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
import Prelude (($), Eq, Maybe, Show, fmap)
import qualified StarTrek.Operations.Magazine.Search.GET.ApiKey as ApiKey
import qualified StarTrek.Operations.Magazine.Search.GET.PageNumber as PageNumber
import qualified StarTrek.Operations.Magazine.Search.GET.PageSize as PageSize
import qualified StarTrek.Types.Error as Error
import qualified StarTrek.Types.MagazineBaseResponse as MagazineBaseResponse

operation ::
  H.Operation
    H.ContentTypeDecodingError
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
      /- "magazine"
      /- "search"

data QueryParams = QueryParams
  { apiKey :: Maybe ApiKey.ApiKey
  , pageNumber :: Maybe PageNumber.PageNumber
  , pageSize :: Maybe PageSize.PageSize
  }
  deriving (Eq, Show)

queryParamsSchema :: H.ParameterCollectionSchema p => p QueryParams QueryParams
queryParamsSchema =
  H.makeParams QueryParams
    ?+ H.optional apiKey ApiKey.paramDef
    ?+ H.optional pageNumber PageNumber.paramDef
    ?+ H.optional pageSize PageSize.paramDef

data Responses
  = Response200 MagazineBaseResponse.MagazineBaseResponse
  | OtherResponse Error.Error
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON MagazineBaseResponse.magazineBaseResponseSchema))
  , (H.AnyStatus, fmap OtherResponse (H.responseBody FA.JSON Error.errorSchema))
  ]