{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams
  ( PathParams(..)
  , route
  , QueryParams(..)
  , queryParamsSchema
  ) where

import Beeline.HTTP.Client ((?+))
import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Data.List.NonEmpty as NEL
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.TestCases.QueryParams.BooleanParam as BooleanParam
import qualified TestCases.Operations.TestCases.QueryParams.InlineEnumParam as InlineEnumParam
import qualified TestCases.Operations.TestCases.QueryParams.OptionalArrayParam as OptionalArrayParam
import qualified TestCases.Operations.TestCases.QueryParams.RequiredArrayParam as RequiredArrayParam
import qualified TestCases.Operations.TestCases.QueryParams.StringParam as StringParam

data PathParams = PathParams
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "test-cases"
      /- "query-params"

data QueryParams = QueryParams
  { booleanParam :: BooleanParam.BooleanParam
  , inlineEnumParam :: Maybe InlineEnumParam.InlineEnumParam
  , optionalArrayParam :: [OptionalArrayParam.OptionalArrayParam]
  , requiredArrayParam :: NEL.NonEmpty RequiredArrayParam.RequiredArrayParam
  , stringParam :: StringParam.StringParam
  }
  deriving (Eq, Show)

queryParamsSchema :: H.QuerySchema q => q QueryParams QueryParams
queryParamsSchema =
  H.makeQuery QueryParams
    ?+ H.required booleanParam BooleanParam.paramDef
    ?+ H.optional inlineEnumParam InlineEnumParam.paramDef
    ?+ H.explodedArray optionalArrayParam OptionalArrayParam.paramDef
    ?+ H.explodedNonEmpty requiredArrayParam RequiredArrayParam.paramDef
    ?+ H.required stringParam StringParam.paramDef