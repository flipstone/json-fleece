{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.ParamSchemaReference
  ( operation
  , PathParams(..)
  , route
  , QueryParams(..)
  , queryParamsSchema
  , Responses(..)
  , responseSchemas
  ) where

import Beeline.HTTP.Client ((?+))
import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/+), (/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Maybe, Show, fmap)
import qualified TestCases.Operations.ParamSchemaReference.EnumIntParam as EnumIntParam
import qualified TestCases.Operations.ParamSchemaReference.EnumParam as EnumParam
import qualified TestCases.Operations.ParamSchemaReference.StringParam as StringParam
import qualified TestCases.Types.EnumIntParam as EnumIntParam
import qualified TestCases.Types.EnumParam as EnumParam
import qualified TestCases.Types.FieldTestCases as FieldTestCases
import qualified TestCases.Types.StringParam as StringParam

operation ::
  H.Operation
    H.ContentTypeDecodingError
    PathParams
    QueryParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestQuerySchema = queryParamsSchema
    , H.responseSchemas = responseSchemas
    }

data PathParams = PathParams
  { stringParam :: StringParam.StringParam
  }
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "test-cases"
      /- "param-ref"
      /+ R.Param StringParam.paramDef stringParam

data QueryParams = QueryParams
  { enumIntParam :: Maybe EnumIntParam.EnumIntParam
  , enumParam :: Maybe EnumParam.EnumParam
  }
  deriving (Eq, Show)

queryParamsSchema :: H.QuerySchema q => q QueryParams QueryParams
queryParamsSchema =
  H.makeQuery QueryParams
    ?+ H.optional enumIntParam EnumIntParam.paramDef
    ?+ H.optional enumParam EnumParam.paramDef

data Responses
  = Response200 FieldTestCases.FieldTestCases
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON FieldTestCases.fieldTestCasesSchema))
  ]