{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.SchemaRefAliasPathParam
  ( operation
  , PathParams(..)
  , route
  , Responses(..)
  , responseSchemas
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/+), (/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Show, fmap)
import qualified TestCases.Operations.SchemaRefAliasPathParam.AliasParam as AStringType
import qualified TestCases.Types.AStringType as AStringType
import qualified TestCases.Types.ObjectWithSchemaRefAliasField as ObjectWithSchemaRefAliasField

operation ::
  H.Operation
    FA.JSONDecodingError
    PathParams
    H.NoQueryParams
    H.NoHeaderParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.responseSchemas = responseSchemas
    }

newtype PathParams = PathParams
  { aliasParam :: AStringType.AStringType
  }
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "test-cases"
      /- "schema-ref-alias-path-param"
      /+ R.Param AStringType.paramDef aliasParam

newtype Responses
  = Response200 ObjectWithSchemaRefAliasField.ObjectWithSchemaRefAliasField
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON ObjectWithSchemaRefAliasField.objectWithSchemaRefAliasFieldSchema))
  ]