{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Me
  ( operation
  , route
  , Responses(..)
  , responseSchemas
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Show, fmap)
import qualified Uber.Types.Error as Error
import qualified Uber.Types.Profile as Profile

operation ::
  H.Operation
    H.ContentTypeDecodingError
    H.NoPathParams
    H.NoQueryParams
    H.NoHeaderParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.get $
    R.make H.NoPathParams
      /- "me"

data Responses
  = Response200 Profile.Profile
  | OtherResponse Error.Error
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON Profile.profileSchema))
  , (H.AnyStatus, fmap OtherResponse (H.responseBody FA.JSON Error.errorSchema))
  ]