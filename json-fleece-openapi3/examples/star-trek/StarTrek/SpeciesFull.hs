{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesFull
  ( SpeciesFull(..)
  , speciesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AstronomicalObjectBase (AstronomicalObjectBase, astronomicalObjectBaseSchema)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.SpeciesFull.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.SpeciesFull.ExtinctSpecies (ExtinctSpecies, extinctSpeciesSchema)
import StarTrek.SpeciesFull.ExtraGalacticSpecies (ExtraGalacticSpecies, extraGalacticSpeciesSchema)
import StarTrek.SpeciesFull.HumanoidSpecies (HumanoidSpecies, humanoidSpeciesSchema)
import StarTrek.SpeciesFull.Name (Name, nameSchema)
import StarTrek.SpeciesFull.NonCorporealSpecies (NonCorporealSpecies, nonCorporealSpeciesSchema)
import StarTrek.SpeciesFull.ReptilianSpecies (ReptilianSpecies, reptilianSpeciesSchema)
import StarTrek.SpeciesFull.ShapeshiftingSpecies (ShapeshiftingSpecies, shapeshiftingSpeciesSchema)
import StarTrek.SpeciesFull.SpaceborneSpecies (SpaceborneSpecies, spaceborneSpeciesSchema)
import StarTrek.SpeciesFull.TelepathicSpecies (TelepathicSpecies, telepathicSpeciesSchema)
import StarTrek.SpeciesFull.TransDimensionalSpecies (TransDimensionalSpecies, transDimensionalSpeciesSchema)
import StarTrek.SpeciesFull.Uid (Uid, uidSchema)
import StarTrek.SpeciesFull.UnnamedSpecies (UnnamedSpecies, unnamedSpeciesSchema)
import StarTrek.SpeciesFull.WarpCapableSpecies (WarpCapableSpecies, warpCapableSpeciesSchema)

data SpeciesFull = SpeciesFull
  { spaceborneSpecies :: Maybe SpaceborneSpecies -- ^ Whether it's a spaceborne species
  , extinctSpecies :: Maybe ExtinctSpecies -- ^ Whether it's an extinct species
  , alternateReality :: Maybe AlternateReality -- ^ Whether this species is from alternate reality
  , name :: Name -- ^ Species name
  , homeworld :: Maybe AstronomicalObjectBase -- ^ Base astronomical object, returned in search results
  , extraGalacticSpecies :: Maybe ExtraGalacticSpecies -- ^ Whether it's an extra-galactic species
  , unnamedSpecies :: Maybe UnnamedSpecies -- ^ Whether it's a unnamed species
  , shapeshiftingSpecies :: Maybe ShapeshiftingSpecies -- ^ Whether it's a shapeshifting species
  , uid :: Uid -- ^ Species unique ID
  , reptilianSpecies :: Maybe ReptilianSpecies -- ^ Whether it's a reptilian species
  , humanoidSpecies :: Maybe HumanoidSpecies -- ^ Whether it's a humanoid species
  , telepathicSpecies :: Maybe TelepathicSpecies -- ^ Whether it's a telepathic species
  , nonCorporealSpecies :: Maybe NonCorporealSpecies -- ^ Whether it's a non-corporeal species
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , quadrant :: Maybe AstronomicalObjectBase -- ^ Base astronomical object, returned in search results
  , transDimensionalSpecies :: Maybe TransDimensionalSpecies -- ^ Whether it's a trans-dimensional species
  , warpCapableSpecies :: Maybe WarpCapableSpecies -- ^ Whether it's a warp-capable species
  }
  deriving (Eq, Show)

speciesFullSchema :: FC.Fleece schema => schema SpeciesFull
speciesFullSchema =
  FC.object $
    FC.constructor SpeciesFull
      #+ FC.optional "spaceborneSpecies" spaceborneSpecies spaceborneSpeciesSchema
      #+ FC.optional "extinctSpecies" extinctSpecies extinctSpeciesSchema
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "homeworld" homeworld astronomicalObjectBaseSchema
      #+ FC.optional "extraGalacticSpecies" extraGalacticSpecies extraGalacticSpeciesSchema
      #+ FC.optional "unnamedSpecies" unnamedSpecies unnamedSpeciesSchema
      #+ FC.optional "shapeshiftingSpecies" shapeshiftingSpecies shapeshiftingSpeciesSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "reptilianSpecies" reptilianSpecies reptilianSpeciesSchema
      #+ FC.optional "humanoidSpecies" humanoidSpecies humanoidSpeciesSchema
      #+ FC.optional "telepathicSpecies" telepathicSpecies telepathicSpeciesSchema
      #+ FC.optional "nonCorporealSpecies" nonCorporealSpecies nonCorporealSpeciesSchema
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "quadrant" quadrant astronomicalObjectBaseSchema
      #+ FC.optional "transDimensionalSpecies" transDimensionalSpecies transDimensionalSpeciesSchema
      #+ FC.optional "warpCapableSpecies" warpCapableSpecies warpCapableSpeciesSchema