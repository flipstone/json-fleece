{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesFull
  ( SpeciesFull(..)
  , speciesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AstronomicalObjectBase as AstronomicalObjectBase
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.SpeciesFull.AlternateReality as AlternateReality
import qualified StarTrek.Types.SpeciesFull.ExtinctSpecies as ExtinctSpecies
import qualified StarTrek.Types.SpeciesFull.ExtraGalacticSpecies as ExtraGalacticSpecies
import qualified StarTrek.Types.SpeciesFull.HumanoidSpecies as HumanoidSpecies
import qualified StarTrek.Types.SpeciesFull.Name as Name
import qualified StarTrek.Types.SpeciesFull.NonCorporealSpecies as NonCorporealSpecies
import qualified StarTrek.Types.SpeciesFull.ReptilianSpecies as ReptilianSpecies
import qualified StarTrek.Types.SpeciesFull.ShapeshiftingSpecies as ShapeshiftingSpecies
import qualified StarTrek.Types.SpeciesFull.SpaceborneSpecies as SpaceborneSpecies
import qualified StarTrek.Types.SpeciesFull.TelepathicSpecies as TelepathicSpecies
import qualified StarTrek.Types.SpeciesFull.TransDimensionalSpecies as TransDimensionalSpecies
import qualified StarTrek.Types.SpeciesFull.Uid as Uid
import qualified StarTrek.Types.SpeciesFull.UnnamedSpecies as UnnamedSpecies
import qualified StarTrek.Types.SpeciesFull.WarpCapableSpecies as WarpCapableSpecies

data SpeciesFull = SpeciesFull
  { spaceborneSpecies :: Maybe SpaceborneSpecies.SpaceborneSpecies -- ^ Whether it's a spaceborne species
  , extinctSpecies :: Maybe ExtinctSpecies.ExtinctSpecies -- ^ Whether it's an extinct species
  , alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this species is from alternate reality
  , name :: Name.Name -- ^ Species name
  , homeworld :: Maybe AstronomicalObjectBase.AstronomicalObjectBase -- ^ Base astronomical object, returned in search results
  , extraGalacticSpecies :: Maybe ExtraGalacticSpecies.ExtraGalacticSpecies -- ^ Whether it's an extra-galactic species
  , unnamedSpecies :: Maybe UnnamedSpecies.UnnamedSpecies -- ^ Whether it's a unnamed species
  , shapeshiftingSpecies :: Maybe ShapeshiftingSpecies.ShapeshiftingSpecies -- ^ Whether it's a shapeshifting species
  , uid :: Uid.Uid -- ^ Species unique ID
  , reptilianSpecies :: Maybe ReptilianSpecies.ReptilianSpecies -- ^ Whether it's a reptilian species
  , humanoidSpecies :: Maybe HumanoidSpecies.HumanoidSpecies -- ^ Whether it's a humanoid species
  , telepathicSpecies :: Maybe TelepathicSpecies.TelepathicSpecies -- ^ Whether it's a telepathic species
  , nonCorporealSpecies :: Maybe NonCorporealSpecies.NonCorporealSpecies -- ^ Whether it's a non-corporeal species
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , quadrant :: Maybe AstronomicalObjectBase.AstronomicalObjectBase -- ^ Base astronomical object, returned in search results
  , transDimensionalSpecies :: Maybe TransDimensionalSpecies.TransDimensionalSpecies -- ^ Whether it's a trans-dimensional species
  , warpCapableSpecies :: Maybe WarpCapableSpecies.WarpCapableSpecies -- ^ Whether it's a warp-capable species
  }
  deriving (Eq, Show)

speciesFullSchema :: FC.Fleece schema => schema SpeciesFull
speciesFullSchema =
  FC.object $
    FC.constructor SpeciesFull
      #+ FC.optional "spaceborneSpecies" spaceborneSpecies SpaceborneSpecies.spaceborneSpeciesSchema
      #+ FC.optional "extinctSpecies" extinctSpecies ExtinctSpecies.extinctSpeciesSchema
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "homeworld" homeworld AstronomicalObjectBase.astronomicalObjectBaseSchema
      #+ FC.optional "extraGalacticSpecies" extraGalacticSpecies ExtraGalacticSpecies.extraGalacticSpeciesSchema
      #+ FC.optional "unnamedSpecies" unnamedSpecies UnnamedSpecies.unnamedSpeciesSchema
      #+ FC.optional "shapeshiftingSpecies" shapeshiftingSpecies ShapeshiftingSpecies.shapeshiftingSpeciesSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "reptilianSpecies" reptilianSpecies ReptilianSpecies.reptilianSpeciesSchema
      #+ FC.optional "humanoidSpecies" humanoidSpecies HumanoidSpecies.humanoidSpeciesSchema
      #+ FC.optional "telepathicSpecies" telepathicSpecies TelepathicSpecies.telepathicSpeciesSchema
      #+ FC.optional "nonCorporealSpecies" nonCorporealSpecies NonCorporealSpecies.nonCorporealSpeciesSchema
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "quadrant" quadrant AstronomicalObjectBase.astronomicalObjectBaseSchema
      #+ FC.optional "transDimensionalSpecies" transDimensionalSpecies TransDimensionalSpecies.transDimensionalSpeciesSchema
      #+ FC.optional "warpCapableSpecies" warpCapableSpecies WarpCapableSpecies.warpCapableSpeciesSchema