{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialFull
  ( MaterialFull(..)
  , materialFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)

data MaterialFull = MaterialFull
  { mineral :: Maybe Bool -- ^ Whether it's a mineral
  , fuel :: Maybe Bool -- ^ Whether it's a fuel
  , name :: Text -- ^ Material name
  , preciousMaterial :: Maybe Bool -- ^ Whether it's a precious material
  , explosive :: Maybe Bool -- ^ Whether it's an explosive
  , chemicalCompound :: Maybe Bool -- ^ Whether it's a chemical compound
  , uid :: Text -- ^ Material unique ID
  , drug :: Maybe Bool -- ^ Whether it's a drug
  , biochemicalCompound :: Maybe Bool -- ^ Whether it's a biochemical compound
  , gemstone :: Maybe Bool -- ^ Whether it's a gemstone
  , poisonousSubstance :: Maybe Bool -- ^ Whether it's a poisonous substance
  , alloyOrComposite :: Maybe Bool -- ^ Whether it's an alloy or a composite
  }
  deriving (Eq, Show)

materialFullSchema :: FC.Fleece schema => schema MaterialFull
materialFullSchema =
  FC.object $
    FC.constructor MaterialFull
      #+ FC.optional "mineral" mineral FC.boolean
      #+ FC.optional "fuel" fuel FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "preciousMaterial" preciousMaterial FC.boolean
      #+ FC.optional "explosive" explosive FC.boolean
      #+ FC.optional "chemicalCompound" chemicalCompound FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "drug" drug FC.boolean
      #+ FC.optional "biochemicalCompound" biochemicalCompound FC.boolean
      #+ FC.optional "gemstone" gemstone FC.boolean
      #+ FC.optional "poisonousSubstance" poisonousSubstance FC.boolean
      #+ FC.optional "alloyOrComposite" alloyOrComposite FC.boolean