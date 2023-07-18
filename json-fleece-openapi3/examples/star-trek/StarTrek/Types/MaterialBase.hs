{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialBase
  ( MaterialBase(..)
  , materialBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MaterialBase.AlloyOrComposite as AlloyOrComposite
import qualified StarTrek.Types.MaterialBase.BiochemicalCompound as BiochemicalCompound
import qualified StarTrek.Types.MaterialBase.ChemicalCompound as ChemicalCompound
import qualified StarTrek.Types.MaterialBase.Drug as Drug
import qualified StarTrek.Types.MaterialBase.Explosive as Explosive
import qualified StarTrek.Types.MaterialBase.Fuel as Fuel
import qualified StarTrek.Types.MaterialBase.Gemstone as Gemstone
import qualified StarTrek.Types.MaterialBase.Mineral as Mineral
import qualified StarTrek.Types.MaterialBase.Name as Name
import qualified StarTrek.Types.MaterialBase.PoisonousSubstance as PoisonousSubstance
import qualified StarTrek.Types.MaterialBase.PreciousMaterial as PreciousMaterial
import qualified StarTrek.Types.MaterialBase.Uid as Uid

data MaterialBase = MaterialBase
  { chemicalCompound :: Maybe ChemicalCompound.ChemicalCompound -- ^ Whether it's a chemical compound
  , drug :: Maybe Drug.Drug -- ^ Whether it's a drug
  , fuel :: Maybe Fuel.Fuel -- ^ Whether it's a fuel
  , preciousMaterial :: Maybe PreciousMaterial.PreciousMaterial -- ^ Whether it's a precious material
  , uid :: Uid.Uid -- ^ Material unique ID
  , poisonousSubstance :: Maybe PoisonousSubstance.PoisonousSubstance -- ^ Whether it's a poisonous substance
  , gemstone :: Maybe Gemstone.Gemstone -- ^ Whether it's a gemstone
  , alloyOrComposite :: Maybe AlloyOrComposite.AlloyOrComposite -- ^ Whether it's an alloy or a composite
  , mineral :: Maybe Mineral.Mineral -- ^ Whether it's a mineral
  , explosive :: Maybe Explosive.Explosive -- ^ Whether it's an explosive
  , biochemicalCompound :: Maybe BiochemicalCompound.BiochemicalCompound -- ^ Whether it's a biochemical compound
  , name :: Name.Name -- ^ Material name
  }
  deriving (Eq, Show)

materialBaseSchema :: FC.Fleece schema => schema MaterialBase
materialBaseSchema =
  FC.object $
    FC.constructor MaterialBase
      #+ FC.optional "chemicalCompound" chemicalCompound ChemicalCompound.chemicalCompoundSchema
      #+ FC.optional "drug" drug Drug.drugSchema
      #+ FC.optional "fuel" fuel Fuel.fuelSchema
      #+ FC.optional "preciousMaterial" preciousMaterial PreciousMaterial.preciousMaterialSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "poisonousSubstance" poisonousSubstance PoisonousSubstance.poisonousSubstanceSchema
      #+ FC.optional "gemstone" gemstone Gemstone.gemstoneSchema
      #+ FC.optional "alloyOrComposite" alloyOrComposite AlloyOrComposite.alloyOrCompositeSchema
      #+ FC.optional "mineral" mineral Mineral.mineralSchema
      #+ FC.optional "explosive" explosive Explosive.explosiveSchema
      #+ FC.optional "biochemicalCompound" biochemicalCompound BiochemicalCompound.biochemicalCompoundSchema
      #+ FC.required "name" name Name.nameSchema