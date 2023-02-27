{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesFull
  ( SpeciesFull(..)
  , speciesFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)
import StarTrek.AstronomicalObjectBase (AstronomicalObjectBase, astronomicalObjectBaseSchema)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)

data SpeciesFull = SpeciesFull
  { spaceborneSpecies :: Maybe Bool -- ^ Whether it's a spaceborne species
  , extinctSpecies :: Maybe Bool -- ^ Whether it's an extinct species
  , alternateReality :: Maybe Bool -- ^ Whether this species is from alternate reality
  , name :: Text -- ^ Species name
  , homeworld :: Maybe AstronomicalObjectBase -- ^ Base astronomical object, returned in search results
  , extraGalacticSpecies :: Maybe Bool -- ^ Whether it's an extra-galactic species
  , unnamedSpecies :: Maybe Bool -- ^ Whether it's a unnamed species
  , shapeshiftingSpecies :: Maybe Bool -- ^ Whether it's a shapeshifting species
  , uid :: Text -- ^ Species unique ID
  , reptilianSpecies :: Maybe Bool -- ^ Whether it's a reptilian species
  , humanoidSpecies :: Maybe Bool -- ^ Whether it's a humanoid species
  , telepathicSpecies :: Maybe Bool -- ^ Whether it's a telepathic species
  , nonCorporealSpecies :: Maybe Bool -- ^ Whether it's a non-corporeal species
  , characters :: Maybe [CharacterBase] -- ^ Characters belonging to the species
  , quadrant :: Maybe AstronomicalObjectBase -- ^ Base astronomical object, returned in search results
  , transDimensionalSpecies :: Maybe Bool -- ^ Whether it's a trans-dimensional species
  , warpCapableSpecies :: Maybe Bool -- ^ Whether it's a warp-capable species
  }
  deriving (Eq, Show)

speciesFullSchema :: FC.Fleece schema => schema SpeciesFull
speciesFullSchema =
  FC.object $
    FC.constructor SpeciesFull
      #+ FC.optional "spaceborneSpecies" spaceborneSpecies FC.boolean
      #+ FC.optional "extinctSpecies" extinctSpecies FC.boolean
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "homeworld" homeworld astronomicalObjectBaseSchema
      #+ FC.optional "extraGalacticSpecies" extraGalacticSpecies FC.boolean
      #+ FC.optional "unnamedSpecies" unnamedSpecies FC.boolean
      #+ FC.optional "shapeshiftingSpecies" shapeshiftingSpecies FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "reptilianSpecies" reptilianSpecies FC.boolean
      #+ FC.optional "humanoidSpecies" humanoidSpecies FC.boolean
      #+ FC.optional "telepathicSpecies" telepathicSpecies FC.boolean
      #+ FC.optional "nonCorporealSpecies" nonCorporealSpecies FC.boolean
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "quadrant" quadrant astronomicalObjectBaseSchema
      #+ FC.optional "transDimensionalSpecies" transDimensionalSpecies FC.boolean
      #+ FC.optional "warpCapableSpecies" warpCapableSpecies FC.boolean