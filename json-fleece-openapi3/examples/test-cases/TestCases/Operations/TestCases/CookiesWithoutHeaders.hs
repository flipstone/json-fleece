{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.CookiesWithoutHeaders
  ( operation
  , route
  , HeaderParams(..)
  , headerParamsSchema
  , CookieParams(..)
  , cookieParamsSchema
  , Responses(..)
  , responseSchemas
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Params ((?+))
import qualified Beeline.Params as P
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Maybe, Show, fmap)
import qualified TestCases.Operations.TestCases.CookiesWithoutHeaders.CookieParamA as CookieParamA
import qualified TestCases.Operations.TestCases.CookiesWithoutHeaders.CookieParamB as CookieParamB
import qualified TestCases.Types.FieldTestCases as FieldTestCases

operation ::
  H.Operation
    FA.JSONDecodingError
    H.NoPathParams
    H.NoQueryParams
    HeaderParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestHeaderSchema = headerParamsSchema
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.get $
    R.make H.NoPathParams
      /- "test-cases"
      /- "cookies-without-headers"

data HeaderParams = HeaderParams
  { cookies :: CookieParams
  }
  deriving (Eq, Show)

headerParamsSchema :: P.HeaderSchema p => p HeaderParams HeaderParams
headerParamsSchema =
  P.makeParams HeaderParams
    ?+ P.cookies cookies cookieParamsSchema

data CookieParams = CookieParams
  { cookieParamA :: CookieParamA.CookieParamA
  , cookieParamB :: Maybe CookieParamB.CookieParamB
  }
  deriving (Eq, Show)

cookieParamsSchema :: P.ParameterSchema p => p CookieParams CookieParams
cookieParamsSchema =
  P.makeParams CookieParams
    ?+ P.required cookieParamA CookieParamA.paramDef
    ?+ P.optional cookieParamB CookieParamB.paramDef

data Responses
  = Response200 FieldTestCases.FieldTestCases
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON FieldTestCases.fieldTestCasesSchema))
  ]