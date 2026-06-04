{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SelectedItemsExample.Operations.GetPet
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
import qualified SelectedItemsExample.Operations.GetPet.PetId as PetId
import qualified SelectedItemsExample.Types.Pet as Pet
import qualified SelectedItemsExample.Types.PetId as PetId

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
  { petId :: PetId.PetId
  }
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "pets"
      /+ R.Param PetId.paramDef petId

newtype Responses
  = Response200 Pet.Pet
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON Pet.petSchema))
  ]