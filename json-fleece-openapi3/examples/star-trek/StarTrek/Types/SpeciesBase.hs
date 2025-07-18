{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesBase
  ( SpeciesBase(..)
  , speciesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AstronomicalObjectHeader as AstronomicalObjectHeader
import qualified StarTrek.Types.SpeciesBase.AlternateReality as AlternateReality
import qualified StarTrek.Types.SpeciesBase.ExtinctSpecies as ExtinctSpecies
import qualified StarTrek.Types.SpeciesBase.ExtraGalacticSpecies as ExtraGalacticSpecies
import qualified StarTrek.Types.SpeciesBase.HumanoidSpecies as HumanoidSpecies
import qualified StarTrek.Types.SpeciesBase.Name as Name
import qualified StarTrek.Types.SpeciesBase.NonCorporealSpecies as NonCorporealSpecies
import qualified StarTrek.Types.SpeciesBase.ReptilianSpecies as ReptilianSpecies
import qualified StarTrek.Types.SpeciesBase.ShapeshiftingSpecies as ShapeshiftingSpecies
import qualified StarTrek.Types.SpeciesBase.SpaceborneSpecies as SpaceborneSpecies
import qualified StarTrek.Types.SpeciesBase.TelepathicSpecies as TelepathicSpecies
import qualified StarTrek.Types.SpeciesBase.TransDimensionalSpecies as TransDimensionalSpecies
import qualified StarTrek.Types.SpeciesBase.Uid as Uid
import qualified StarTrek.Types.SpeciesBase.UnnamedSpecies as UnnamedSpecies
import qualified StarTrek.Types.SpeciesBase.WarpCapableSpecies as WarpCapableSpecies

data SpeciesBase = SpeciesBase
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this species is from alternate reality
  , extinctSpecies :: Maybe ExtinctSpecies.ExtinctSpecies -- ^ Whether it's an extinct species
  , extraGalacticSpecies :: Maybe ExtraGalacticSpecies.ExtraGalacticSpecies -- ^ Whether it's an extra-galactic species
  , homeworld :: Maybe AstronomicalObjectHeader.AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  , humanoidSpecies :: Maybe HumanoidSpecies.HumanoidSpecies -- ^ Whether it's a humanoid species
  , name :: Name.Name -- ^ Species name
  , nonCorporealSpecies :: Maybe NonCorporealSpecies.NonCorporealSpecies -- ^ Whether it's a non-corporeal species
  , quadrant :: Maybe AstronomicalObjectHeader.AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  , reptilianSpecies :: Maybe ReptilianSpecies.ReptilianSpecies -- ^ Whether it's a reptilian species
  , shapeshiftingSpecies :: Maybe ShapeshiftingSpecies.ShapeshiftingSpecies -- ^ Whether it's a shapeshifting species
  , spaceborneSpecies :: Maybe SpaceborneSpecies.SpaceborneSpecies -- ^ Whether it's a spaceborne species
  , telepathicSpecies :: Maybe TelepathicSpecies.TelepathicSpecies -- ^ Whether it's a telepathic species
  , transDimensionalSpecies :: Maybe TransDimensionalSpecies.TransDimensionalSpecies -- ^ Whether it's a trans-dimensional species
  , uid :: Uid.Uid -- ^ Species unique ID
  , unnamedSpecies :: Maybe UnnamedSpecies.UnnamedSpecies -- ^ Whether it's a unnamed species
  , warpCapableSpecies :: Maybe WarpCapableSpecies.WarpCapableSpecies -- ^ Whether it's a warp-capable species
  }
  deriving (Eq, Show)

speciesBaseSchema :: FC.Fleece schema => schema SpeciesBase
speciesBaseSchema =
  FC.object $
    FC.constructor SpeciesBase
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "extinctSpecies" extinctSpecies ExtinctSpecies.extinctSpeciesSchema
      #+ FC.optional "extraGalacticSpecies" extraGalacticSpecies ExtraGalacticSpecies.extraGalacticSpeciesSchema
      #+ FC.optional "homeworld" homeworld AstronomicalObjectHeader.astronomicalObjectHeaderSchema
      #+ FC.optional "humanoidSpecies" humanoidSpecies HumanoidSpecies.humanoidSpeciesSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "nonCorporealSpecies" nonCorporealSpecies NonCorporealSpecies.nonCorporealSpeciesSchema
      #+ FC.optional "quadrant" quadrant AstronomicalObjectHeader.astronomicalObjectHeaderSchema
      #+ FC.optional "reptilianSpecies" reptilianSpecies ReptilianSpecies.reptilianSpeciesSchema
      #+ FC.optional "shapeshiftingSpecies" shapeshiftingSpecies ShapeshiftingSpecies.shapeshiftingSpeciesSchema
      #+ FC.optional "spaceborneSpecies" spaceborneSpecies SpaceborneSpecies.spaceborneSpeciesSchema
      #+ FC.optional "telepathicSpecies" telepathicSpecies TelepathicSpecies.telepathicSpeciesSchema
      #+ FC.optional "transDimensionalSpecies" transDimensionalSpecies TransDimensionalSpecies.transDimensionalSpeciesSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "unnamedSpecies" unnamedSpecies UnnamedSpecies.unnamedSpeciesSchema
      #+ FC.optional "warpCapableSpecies" warpCapableSpecies WarpCapableSpecies.warpCapableSpeciesSchema