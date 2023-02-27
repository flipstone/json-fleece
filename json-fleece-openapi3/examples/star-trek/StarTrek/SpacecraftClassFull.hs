{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassFull
  ( SpacecraftClassFull(..)
  , spacecraftClassFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.OrganizationBase (OrganizationBase, organizationBaseSchema)
import StarTrek.SpacecraftBase (SpacecraftBase, spacecraftBaseSchema)
import StarTrek.SpacecraftType (SpacecraftType, spacecraftTypeSchema)
import StarTrek.SpeciesHeader (SpeciesHeader, speciesHeaderSchema)

data SpacecraftClassFull = SpacecraftClassFull
  { alternateReality :: Maybe Bool -- ^ Whether this spacecraft class is from alternate reality
  , name :: Text -- ^ Spacecraft class name
  , spacecrafts :: Maybe [SpacecraftBase] -- ^ Spacecrafts
  , activeFrom :: Maybe Text -- ^ Starting period when this spacecraft class was in use
  , warpCapable :: Maybe Bool -- ^ Whether it's a warp-capable spacecraft class
  , numberOfDecks :: Maybe Integer -- ^ Number of decks
  , affiliation :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , uid :: Text -- ^ Spacecraft class unique ID
  , owner :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , species :: Maybe SpeciesHeader -- ^ Header species, embedded in other objects
  , activeTo :: Maybe Text -- ^ Ending period when this spacecraft class was in use
  , operator :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , spacecraftTypes :: Maybe [SpacecraftType] -- ^ Spacecraft types
  }
  deriving (Eq, Show)

spacecraftClassFullSchema :: FC.Fleece schema => schema SpacecraftClassFull
spacecraftClassFullSchema =
  FC.object $
    FC.constructor SpacecraftClassFull
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "spacecrafts" spacecrafts (FC.list spacecraftBaseSchema)
      #+ FC.optional "activeFrom" activeFrom FC.text
      #+ FC.optional "warpCapable" warpCapable FC.boolean
      #+ FC.optional "numberOfDecks" numberOfDecks FC.integer
      #+ FC.optional "affiliation" affiliation organizationBaseSchema
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "owner" owner organizationBaseSchema
      #+ FC.optional "species" species speciesHeaderSchema
      #+ FC.optional "activeTo" activeTo FC.text
      #+ FC.optional "operator" operator organizationBaseSchema
      #+ FC.optional "spacecraftTypes" spacecraftTypes (FC.list spacecraftTypeSchema)