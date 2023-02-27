{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialFull
  ( MaterialFull(..)
  , materialFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MaterialFull.AlloyOrComposite (AlloyOrComposite, alloyOrCompositeSchema)
import StarTrek.MaterialFull.BiochemicalCompound (BiochemicalCompound, biochemicalCompoundSchema)
import StarTrek.MaterialFull.ChemicalCompound (ChemicalCompound, chemicalCompoundSchema)
import StarTrek.MaterialFull.Drug (Drug, drugSchema)
import StarTrek.MaterialFull.Explosive (Explosive, explosiveSchema)
import StarTrek.MaterialFull.Fuel (Fuel, fuelSchema)
import StarTrek.MaterialFull.Gemstone (Gemstone, gemstoneSchema)
import StarTrek.MaterialFull.Mineral (Mineral, mineralSchema)
import StarTrek.MaterialFull.Name (Name, nameSchema)
import StarTrek.MaterialFull.PoisonousSubstance (PoisonousSubstance, poisonousSubstanceSchema)
import StarTrek.MaterialFull.PreciousMaterial (PreciousMaterial, preciousMaterialSchema)
import StarTrek.MaterialFull.Uid (Uid, uidSchema)

data MaterialFull = MaterialFull
  { mineral :: Maybe Mineral -- ^ Whether it's a mineral
  , fuel :: Maybe Fuel -- ^ Whether it's a fuel
  , name :: Name -- ^ Material name
  , preciousMaterial :: Maybe PreciousMaterial -- ^ Whether it's a precious material
  , explosive :: Maybe Explosive -- ^ Whether it's an explosive
  , chemicalCompound :: Maybe ChemicalCompound -- ^ Whether it's a chemical compound
  , uid :: Uid -- ^ Material unique ID
  , drug :: Maybe Drug -- ^ Whether it's a drug
  , biochemicalCompound :: Maybe BiochemicalCompound -- ^ Whether it's a biochemical compound
  , gemstone :: Maybe Gemstone -- ^ Whether it's a gemstone
  , poisonousSubstance :: Maybe PoisonousSubstance -- ^ Whether it's a poisonous substance
  , alloyOrComposite :: Maybe AlloyOrComposite -- ^ Whether it's an alloy or a composite
  }
  deriving (Eq, Show)

materialFullSchema :: FC.Fleece schema => schema MaterialFull
materialFullSchema =
  FC.object $
    FC.constructor MaterialFull
      #+ FC.optional "mineral" mineral mineralSchema
      #+ FC.optional "fuel" fuel fuelSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "preciousMaterial" preciousMaterial preciousMaterialSchema
      #+ FC.optional "explosive" explosive explosiveSchema
      #+ FC.optional "chemicalCompound" chemicalCompound chemicalCompoundSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "drug" drug drugSchema
      #+ FC.optional "biochemicalCompound" biochemicalCompound biochemicalCompoundSchema
      #+ FC.optional "gemstone" gemstone gemstoneSchema
      #+ FC.optional "poisonousSubstance" poisonousSubstance poisonousSubstanceSchema
      #+ FC.optional "alloyOrComposite" alloyOrComposite alloyOrCompositeSchema