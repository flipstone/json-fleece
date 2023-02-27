{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesBase
  ( SpeciesBase(..)
  , speciesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AstronomicalObjectHeader (AstronomicalObjectHeader, astronomicalObjectHeaderSchema)
import StarTrek.SpeciesBase.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.SpeciesBase.ExtinctSpecies (ExtinctSpecies, extinctSpeciesSchema)
import StarTrek.SpeciesBase.ExtraGalacticSpecies (ExtraGalacticSpecies, extraGalacticSpeciesSchema)
import StarTrek.SpeciesBase.HumanoidSpecies (HumanoidSpecies, humanoidSpeciesSchema)
import StarTrek.SpeciesBase.Name (Name, nameSchema)
import StarTrek.SpeciesBase.NonCorporealSpecies (NonCorporealSpecies, nonCorporealSpeciesSchema)
import StarTrek.SpeciesBase.ReptilianSpecies (ReptilianSpecies, reptilianSpeciesSchema)
import StarTrek.SpeciesBase.ShapeshiftingSpecies (ShapeshiftingSpecies, shapeshiftingSpeciesSchema)
import StarTrek.SpeciesBase.SpaceborneSpecies (SpaceborneSpecies, spaceborneSpeciesSchema)
import StarTrek.SpeciesBase.TelepathicSpecies (TelepathicSpecies, telepathicSpeciesSchema)
import StarTrek.SpeciesBase.TransDimensionalSpecies (TransDimensionalSpecies, transDimensionalSpeciesSchema)
import StarTrek.SpeciesBase.Uid (Uid, uidSchema)
import StarTrek.SpeciesBase.UnnamedSpecies (UnnamedSpecies, unnamedSpeciesSchema)
import StarTrek.SpeciesBase.WarpCapableSpecies (WarpCapableSpecies, warpCapableSpeciesSchema)

data SpeciesBase = SpeciesBase
  { spaceborneSpecies :: Maybe SpaceborneSpecies -- ^ Whether it's a spaceborne species
  , extinctSpecies :: Maybe ExtinctSpecies -- ^ Whether it's an extinct species
  , alternateReality :: Maybe AlternateReality -- ^ Whether this species is from alternate reality
  , name :: Name -- ^ Species name
  , homeworld :: Maybe AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  , extraGalacticSpecies :: Maybe ExtraGalacticSpecies -- ^ Whether it's an extra-galactic species
  , unnamedSpecies :: Maybe UnnamedSpecies -- ^ Whether it's a unnamed species
  , shapeshiftingSpecies :: Maybe ShapeshiftingSpecies -- ^ Whether it's a shapeshifting species
  , uid :: Uid -- ^ Species unique ID
  , reptilianSpecies :: Maybe ReptilianSpecies -- ^ Whether it's a reptilian species
  , humanoidSpecies :: Maybe HumanoidSpecies -- ^ Whether it's a humanoid species
  , telepathicSpecies :: Maybe TelepathicSpecies -- ^ Whether it's a telepathic species
  , nonCorporealSpecies :: Maybe NonCorporealSpecies -- ^ Whether it's a non-corporeal species
  , quadrant :: Maybe AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  , transDimensionalSpecies :: Maybe TransDimensionalSpecies -- ^ Whether it's a trans-dimensional species
  , warpCapableSpecies :: Maybe WarpCapableSpecies -- ^ Whether it's a warp-capable species
  }
  deriving (Eq, Show)

speciesBaseSchema :: FC.Fleece schema => schema SpeciesBase
speciesBaseSchema =
  FC.object $
    FC.constructor SpeciesBase
      #+ FC.optional "spaceborneSpecies" spaceborneSpecies spaceborneSpeciesSchema
      #+ FC.optional "extinctSpecies" extinctSpecies extinctSpeciesSchema
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "homeworld" homeworld astronomicalObjectHeaderSchema
      #+ FC.optional "extraGalacticSpecies" extraGalacticSpecies extraGalacticSpeciesSchema
      #+ FC.optional "unnamedSpecies" unnamedSpecies unnamedSpeciesSchema
      #+ FC.optional "shapeshiftingSpecies" shapeshiftingSpecies shapeshiftingSpeciesSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "reptilianSpecies" reptilianSpecies reptilianSpeciesSchema
      #+ FC.optional "humanoidSpecies" humanoidSpecies humanoidSpeciesSchema
      #+ FC.optional "telepathicSpecies" telepathicSpecies telepathicSpeciesSchema
      #+ FC.optional "nonCorporealSpecies" nonCorporealSpecies nonCorporealSpeciesSchema
      #+ FC.optional "quadrant" quadrant astronomicalObjectHeaderSchema
      #+ FC.optional "transDimensionalSpecies" transDimensionalSpecies transDimensionalSpeciesSchema
      #+ FC.optional "warpCapableSpecies" warpCapableSpecies warpCapableSpeciesSchema