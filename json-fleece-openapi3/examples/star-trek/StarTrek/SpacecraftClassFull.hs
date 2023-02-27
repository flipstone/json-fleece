{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassFull
  ( SpacecraftClassFull(..)
  , spacecraftClassFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OrganizationBase (OrganizationBase, organizationBaseSchema)
import StarTrek.SpacecraftBase (SpacecraftBase, spacecraftBaseSchema)
import StarTrek.SpacecraftClassFull.ActiveFrom (ActiveFrom, activeFromSchema)
import StarTrek.SpacecraftClassFull.ActiveTo (ActiveTo, activeToSchema)
import StarTrek.SpacecraftClassFull.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.SpacecraftClassFull.Name (Name, nameSchema)
import StarTrek.SpacecraftClassFull.NumberOfDecks (NumberOfDecks, numberOfDecksSchema)
import StarTrek.SpacecraftClassFull.Uid (Uid, uidSchema)
import StarTrek.SpacecraftClassFull.WarpCapable (WarpCapable, warpCapableSchema)
import StarTrek.SpacecraftType (SpacecraftType, spacecraftTypeSchema)
import StarTrek.SpeciesHeader (SpeciesHeader, speciesHeaderSchema)

data SpacecraftClassFull = SpacecraftClassFull
  { alternateReality :: Maybe AlternateReality -- ^ Whether this spacecraft class is from alternate reality
  , name :: Name -- ^ Spacecraft class name
  , spacecrafts :: Maybe [SpacecraftBase] -- ^ Base spacecraft, returned in search results
  , activeFrom :: Maybe ActiveFrom -- ^ Starting period when this spacecraft class was in use
  , warpCapable :: Maybe WarpCapable -- ^ Whether it's a warp-capable spacecraft class
  , numberOfDecks :: Maybe NumberOfDecks -- ^ Number of decks
  , affiliation :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , uid :: Uid -- ^ Spacecraft class unique ID
  , owner :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , species :: Maybe SpeciesHeader -- ^ Header species, embedded in other objects
  , activeTo :: Maybe ActiveTo -- ^ Ending period when this spacecraft class was in use
  , operator :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , spacecraftTypes :: Maybe [SpacecraftType] -- ^ Rating of video release, etc.
  }
  deriving (Eq, Show)

spacecraftClassFullSchema :: FC.Fleece schema => schema SpacecraftClassFull
spacecraftClassFullSchema =
  FC.object $
    FC.constructor SpacecraftClassFull
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "spacecrafts" spacecrafts (FC.list spacecraftBaseSchema)
      #+ FC.optional "activeFrom" activeFrom activeFromSchema
      #+ FC.optional "warpCapable" warpCapable warpCapableSchema
      #+ FC.optional "numberOfDecks" numberOfDecks numberOfDecksSchema
      #+ FC.optional "affiliation" affiliation organizationBaseSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "owner" owner organizationBaseSchema
      #+ FC.optional "species" species speciesHeaderSchema
      #+ FC.optional "activeTo" activeTo activeToSchema
      #+ FC.optional "operator" operator organizationBaseSchema
      #+ FC.optional "spacecraftTypes" spacecraftTypes (FC.list spacecraftTypeSchema)