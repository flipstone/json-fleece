{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SelectedItemsExample.Operations.CreatePet
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
import qualified SelectedItemsExample.Types.NewPet as NewPet
import qualified SelectedItemsExample.Types.Pet as Pet

operation ::
  H.Operation
    FA.JSONDecodingError
    H.NoPathParams
    H.NoQueryParams
    H.NoHeaderParams
    NewPet.NewPet
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestBodySchema = H.requestBody FA.JSON NewPet.newPetSchema
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.post $
    R.make H.NoPathParams
      /- "pets"

newtype Responses
  = Response201 Pet.Pet
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 201, fmap Response201 (H.responseBody FA.JSON Pet.petSchema))
  ]