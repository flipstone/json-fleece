{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.TradingCardDeck
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
import qualified StarTrek.Operations.TradingCardDeck.ApiKey as ApiKey
import qualified StarTrek.Operations.TradingCardDeck.Uid as Uid
import qualified StarTrek.Types.Error as Error
import qualified StarTrek.Types.TradingCardDeckFullResponse as TradingCardDeckFullResponse

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
      /- "tradingCardDeck"

data QueryParams = QueryParams
  { apiKey :: Maybe ApiKey.ApiKey
  , uid :: Uid.Uid
  }
  deriving (Eq, Show)

queryParamsSchema :: H.ParameterCollectionSchema p => p QueryParams QueryParams
queryParamsSchema =
  H.makeParams QueryParams
    ?+ H.optional apiKey ApiKey.paramDef
    ?+ H.required uid Uid.paramDef

data Responses
  = Response200 TradingCardDeckFullResponse.TradingCardDeckFullResponse
  | OtherResponse Error.Error
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON TradingCardDeckFullResponse.tradingCardDeckFullResponseSchema))
  , (H.AnyStatus, fmap OtherResponse (H.responseBody FA.JSON Error.errorSchema))
  ]