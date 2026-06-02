{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module SelectedItemsExample.Types.Habitat
  ( Habitat(..)
  , habitatSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified SelectedItemsExample.Operations.Habitat.Option1 as Option1
import qualified SelectedItemsExample.Operations.Habitat.Option2 as Option2
import qualified Shrubbery as Shrubbery

newtype Habitat = Habitat (Shrubbery.Union
  '[ Option1.Option1
   , Option2.Option2
   ])
  deriving (Show, Eq)

habitatSchema :: FC.Fleece t => FC.Schema t Habitat
habitatSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "SelectedItemsExample.Types.Habitat" "Habitat") $
      FC.unionMember Option1.option1Schema
        #| FC.unionMember Option2.option2Schema