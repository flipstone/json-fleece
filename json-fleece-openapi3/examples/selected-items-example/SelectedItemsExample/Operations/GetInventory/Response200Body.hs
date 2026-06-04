{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Operations.GetInventory.Response200Body
  ( Response200Body(..)
  , response200BodySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified SelectedItemsExample.Operations.GetInventory.Response200Body.Meta as Meta
import qualified SelectedItemsExample.Operations.GetInventory.Response200Body.Total as Total
import qualified SelectedItemsExample.Types.Pet as Pet

data Response200Body = Response200Body
  { meta :: Meta.Meta
  , topItem :: Maybe Pet.Pet
  , total :: Total.Total
  }
  deriving (Eq, Show)

response200BodySchema :: FC.Fleece t => FC.Schema t Response200Body
response200BodySchema =
  FC.object $
    FC.constructor Response200Body
      #+ FC.required "meta" meta Meta.metaSchema
      #+ FC.optional "topItem" topItem Pet.petSchema
      #+ FC.required "total" total Total.totalSchema