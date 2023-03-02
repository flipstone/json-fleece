{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesBase
  ( SpeciesBase(..)
  , speciesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.AstronomicalObjectHeader as AstronomicalObjectHeader
import qualified StarTrek.SpeciesBase.AlternateReality as AlternateReality
import qualified StarTrek.SpeciesBase.ExtinctSpecies as ExtinctSpecies
import qualified StarTrek.SpeciesBase.ExtraGalacticSpecies as ExtraGalacticSpecies
import qualified StarTrek.SpeciesBase.HumanoidSpecies as HumanoidSpecies
import qualified StarTrek.SpeciesBase.Name as Name
import qualified StarTrek.SpeciesBase.NonCorporealSpecies as NonCorporealSpecies
import qualified StarTrek.SpeciesBase.ReptilianSpecies as ReptilianSpecies
import qualified StarTrek.SpeciesBase.ShapeshiftingSpecies as ShapeshiftingSpecies
import qualified StarTrek.SpeciesBase.SpaceborneSpecies as SpaceborneSpecies
import qualified StarTrek.SpeciesBase.TelepathicSpecies as TelepathicSpecies
import qualified StarTrek.SpeciesBase.TransDimensionalSpecies as TransDimensionalSpecies
import qualified StarTrek.SpeciesBase.Uid as Uid
import qualified StarTrek.SpeciesBase.UnnamedSpecies as UnnamedSpecies
import qualified StarTrek.SpeciesBase.WarpCapableSpecies as WarpCapableSpecies

data SpeciesBase = SpeciesBase
  { spaceborneSpecies :: Maybe SpaceborneSpecies.SpaceborneSpecies -- ^ Whether it's a spaceborne species
  , extinctSpecies :: Maybe ExtinctSpecies.ExtinctSpecies -- ^ Whether it's an extinct species
  , alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this species is from alternate reality
  , name :: Name.Name -- ^ Species name
  , homeworld :: Maybe AstronomicalObjectHeader.AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  , extraGalacticSpecies :: Maybe ExtraGalacticSpecies.ExtraGalacticSpecies -- ^ Whether it's an extra-galactic species
  , unnamedSpecies :: Maybe UnnamedSpecies.UnnamedSpecies -- ^ Whether it's a unnamed species
  , shapeshiftingSpecies :: Maybe ShapeshiftingSpecies.ShapeshiftingSpecies -- ^ Whether it's a shapeshifting species
  , uid :: Uid.Uid -- ^ Species unique ID
  , reptilianSpecies :: Maybe ReptilianSpecies.ReptilianSpecies -- ^ Whether it's a reptilian species
  , humanoidSpecies :: Maybe HumanoidSpecies.HumanoidSpecies -- ^ Whether it's a humanoid species
  , telepathicSpecies :: Maybe TelepathicSpecies.TelepathicSpecies -- ^ Whether it's a telepathic species
  , nonCorporealSpecies :: Maybe NonCorporealSpecies.NonCorporealSpecies -- ^ Whether it's a non-corporeal species
  , quadrant :: Maybe AstronomicalObjectHeader.AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  , transDimensionalSpecies :: Maybe TransDimensionalSpecies.TransDimensionalSpecies -- ^ Whether it's a trans-dimensional species
  , warpCapableSpecies :: Maybe WarpCapableSpecies.WarpCapableSpecies -- ^ Whether it's a warp-capable species
  }
  deriving (Eq, Show)

speciesBaseSchema :: FC.Fleece schema => schema SpeciesBase
speciesBaseSchema =
  FC.object $
    FC.constructor SpeciesBase
      #+ FC.optional "spaceborneSpecies" spaceborneSpecies SpaceborneSpecies.spaceborneSpeciesSchema
      #+ FC.optional "extinctSpecies" extinctSpecies ExtinctSpecies.extinctSpeciesSchema
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "homeworld" homeworld AstronomicalObjectHeader.astronomicalObjectHeaderSchema
      #+ FC.optional "extraGalacticSpecies" extraGalacticSpecies ExtraGalacticSpecies.extraGalacticSpeciesSchema
      #+ FC.optional "unnamedSpecies" unnamedSpecies UnnamedSpecies.unnamedSpeciesSchema
      #+ FC.optional "shapeshiftingSpecies" shapeshiftingSpecies ShapeshiftingSpecies.shapeshiftingSpeciesSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "reptilianSpecies" reptilianSpecies ReptilianSpecies.reptilianSpeciesSchema
      #+ FC.optional "humanoidSpecies" humanoidSpecies HumanoidSpecies.humanoidSpeciesSchema
      #+ FC.optional "telepathicSpecies" telepathicSpecies TelepathicSpecies.telepathicSpeciesSchema
      #+ FC.optional "nonCorporealSpecies" nonCorporealSpecies NonCorporealSpecies.nonCorporealSpeciesSchema
      #+ FC.optional "quadrant" quadrant AstronomicalObjectHeader.astronomicalObjectHeaderSchema
      #+ FC.optional "transDimensionalSpecies" transDimensionalSpecies TransDimensionalSpecies.transDimensionalSpeciesSchema
      #+ FC.optional "warpCapableSpecies" warpCapableSpecies WarpCapableSpecies.warpCapableSpeciesSchema