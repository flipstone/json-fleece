{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialFull
  ( MaterialFull(..)
  , materialFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MaterialFull.AlloyOrComposite as AlloyOrComposite
import qualified StarTrek.Types.MaterialFull.BiochemicalCompound as BiochemicalCompound
import qualified StarTrek.Types.MaterialFull.ChemicalCompound as ChemicalCompound
import qualified StarTrek.Types.MaterialFull.Drug as Drug
import qualified StarTrek.Types.MaterialFull.Explosive as Explosive
import qualified StarTrek.Types.MaterialFull.Fuel as Fuel
import qualified StarTrek.Types.MaterialFull.Gemstone as Gemstone
import qualified StarTrek.Types.MaterialFull.Mineral as Mineral
import qualified StarTrek.Types.MaterialFull.Name as Name
import qualified StarTrek.Types.MaterialFull.PoisonousSubstance as PoisonousSubstance
import qualified StarTrek.Types.MaterialFull.PreciousMaterial as PreciousMaterial
import qualified StarTrek.Types.MaterialFull.Uid as Uid

data MaterialFull = MaterialFull
  { alloyOrComposite :: Maybe AlloyOrComposite.AlloyOrComposite -- ^ Whether it's an alloy or a composite
  , biochemicalCompound :: Maybe BiochemicalCompound.BiochemicalCompound -- ^ Whether it's a biochemical compound
  , chemicalCompound :: Maybe ChemicalCompound.ChemicalCompound -- ^ Whether it's a chemical compound
  , drug :: Maybe Drug.Drug -- ^ Whether it's a drug
  , explosive :: Maybe Explosive.Explosive -- ^ Whether it's an explosive
  , fuel :: Maybe Fuel.Fuel -- ^ Whether it's a fuel
  , gemstone :: Maybe Gemstone.Gemstone -- ^ Whether it's a gemstone
  , mineral :: Maybe Mineral.Mineral -- ^ Whether it's a mineral
  , name :: Name.Name -- ^ Material name
  , poisonousSubstance :: Maybe PoisonousSubstance.PoisonousSubstance -- ^ Whether it's a poisonous substance
  , preciousMaterial :: Maybe PreciousMaterial.PreciousMaterial -- ^ Whether it's a precious material
  , uid :: Uid.Uid -- ^ Material unique ID
  }
  deriving (Eq, Show)

materialFullSchema :: FC.Fleece t => FC.Schema t MaterialFull
materialFullSchema =
  FC.object $
    FC.constructor MaterialFull
      #+ FC.optional "alloyOrComposite" alloyOrComposite AlloyOrComposite.alloyOrCompositeSchema
      #+ FC.optional "biochemicalCompound" biochemicalCompound BiochemicalCompound.biochemicalCompoundSchema
      #+ FC.optional "chemicalCompound" chemicalCompound ChemicalCompound.chemicalCompoundSchema
      #+ FC.optional "drug" drug Drug.drugSchema
      #+ FC.optional "explosive" explosive Explosive.explosiveSchema
      #+ FC.optional "fuel" fuel Fuel.fuelSchema
      #+ FC.optional "gemstone" gemstone Gemstone.gemstoneSchema
      #+ FC.optional "mineral" mineral Mineral.mineralSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "poisonousSubstance" poisonousSubstance PoisonousSubstance.poisonousSubstanceSchema
      #+ FC.optional "preciousMaterial" preciousMaterial PreciousMaterial.preciousMaterialSchema
      #+ FC.required "uid" uid Uid.uidSchema