{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SelectedItemsExample.Operations.GetPet.PetId
  ( paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified SelectedItemsExample.Types.PetId as PetId

paramDef :: R.ParameterDefinition PetId.PetId
paramDef =
  P.coerceParam (P.int32Param "petId")