{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialBase
  ( MaterialBase(..)
  , materialBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MaterialBase.AlloyOrComposite (AlloyOrComposite, alloyOrCompositeSchema)
import StarTrek.MaterialBase.BiochemicalCompound (BiochemicalCompound, biochemicalCompoundSchema)
import StarTrek.MaterialBase.ChemicalCompound (ChemicalCompound, chemicalCompoundSchema)
import StarTrek.MaterialBase.Drug (Drug, drugSchema)
import StarTrek.MaterialBase.Explosive (Explosive, explosiveSchema)
import StarTrek.MaterialBase.Fuel (Fuel, fuelSchema)
import StarTrek.MaterialBase.Gemstone (Gemstone, gemstoneSchema)
import StarTrek.MaterialBase.Mineral (Mineral, mineralSchema)
import StarTrek.MaterialBase.Name (Name, nameSchema)
import StarTrek.MaterialBase.PoisonousSubstance (PoisonousSubstance, poisonousSubstanceSchema)
import StarTrek.MaterialBase.PreciousMaterial (PreciousMaterial, preciousMaterialSchema)
import StarTrek.MaterialBase.Uid (Uid, uidSchema)

data MaterialBase = MaterialBase
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

materialBaseSchema :: FC.Fleece schema => schema MaterialBase
materialBaseSchema =
  FC.object $
    FC.constructor MaterialBase
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