{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesBase
  ( SpeciesBase(..)
  , speciesBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)
import StarTrek.AstronomicalObjectHeader (AstronomicalObjectHeader, astronomicalObjectHeaderSchema)

data SpeciesBase = SpeciesBase
  { spaceborneSpecies :: Maybe Bool -- ^ Whether it's a spaceborne species
  , extinctSpecies :: Maybe Bool -- ^ Whether it's an extinct species
  , alternateReality :: Maybe Bool -- ^ Whether this species is from alternate reality
  , name :: Text -- ^ Species name
  , homeworld :: Maybe AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  , extraGalacticSpecies :: Maybe Bool -- ^ Whether it's an extra-galactic species
  , unnamedSpecies :: Maybe Bool -- ^ Whether it's a unnamed species
  , shapeshiftingSpecies :: Maybe Bool -- ^ Whether it's a shapeshifting species
  , uid :: Text -- ^ Species unique ID
  , reptilianSpecies :: Maybe Bool -- ^ Whether it's a reptilian species
  , humanoidSpecies :: Maybe Bool -- ^ Whether it's a humanoid species
  , telepathicSpecies :: Maybe Bool -- ^ Whether it's a telepathic species
  , nonCorporealSpecies :: Maybe Bool -- ^ Whether it's a non-corporeal species
  , quadrant :: Maybe AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  , transDimensionalSpecies :: Maybe Bool -- ^ Whether it's a trans-dimensional species
  , warpCapableSpecies :: Maybe Bool -- ^ Whether it's a warp-capable species
  }
  deriving (Eq, Show)

speciesBaseSchema :: FC.Fleece schema => schema SpeciesBase
speciesBaseSchema =
  FC.object $
    FC.constructor SpeciesBase
      #+ FC.optional "spaceborneSpecies" spaceborneSpecies FC.boolean
      #+ FC.optional "extinctSpecies" extinctSpecies FC.boolean
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "homeworld" homeworld astronomicalObjectHeaderSchema
      #+ FC.optional "extraGalacticSpecies" extraGalacticSpecies FC.boolean
      #+ FC.optional "unnamedSpecies" unnamedSpecies FC.boolean
      #+ FC.optional "shapeshiftingSpecies" shapeshiftingSpecies FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "reptilianSpecies" reptilianSpecies FC.boolean
      #+ FC.optional "humanoidSpecies" humanoidSpecies FC.boolean
      #+ FC.optional "telepathicSpecies" telepathicSpecies FC.boolean
      #+ FC.optional "nonCorporealSpecies" nonCorporealSpecies FC.boolean
      #+ FC.optional "quadrant" quadrant astronomicalObjectHeaderSchema
      #+ FC.optional "transDimensionalSpecies" transDimensionalSpecies FC.boolean
      #+ FC.optional "warpCapableSpecies" warpCapableSpecies FC.boolean