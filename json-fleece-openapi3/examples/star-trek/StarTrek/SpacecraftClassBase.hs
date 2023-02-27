{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassBase
  ( SpacecraftClassBase(..)
  , spacecraftClassBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OrganizationHeader (OrganizationHeader, organizationHeaderSchema)
import StarTrek.SpacecraftClassBase.ActiveFrom (ActiveFrom, activeFromSchema)
import StarTrek.SpacecraftClassBase.ActiveTo (ActiveTo, activeToSchema)
import StarTrek.SpacecraftClassBase.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.SpacecraftClassBase.Name (Name, nameSchema)
import StarTrek.SpacecraftClassBase.NumberOfDecks (NumberOfDecks, numberOfDecksSchema)
import StarTrek.SpacecraftClassBase.Uid (Uid, uidSchema)
import StarTrek.SpacecraftClassBase.WarpCapable (WarpCapable, warpCapableSchema)
import StarTrek.SpeciesHeader (SpeciesHeader, speciesHeaderSchema)

data SpacecraftClassBase = SpacecraftClassBase
  { alternateReality :: Maybe AlternateReality -- ^ Whether this spacecraft class is from alternate reality
  , name :: Name -- ^ Spacecraft class name
  , activeFrom :: Maybe ActiveFrom -- ^ Starting period when this spacecraft class was in use
  , warpCapable :: Maybe WarpCapable -- ^ Whether it's a warp-capable spacecraft class
  , numberOfDecks :: Maybe NumberOfDecks -- ^ Number of decks
  , affiliation :: Maybe OrganizationHeader -- ^ Header organization, embedded in other objects
  , uid :: Uid -- ^ Spacecraft class unique ID
  , owner :: Maybe OrganizationHeader -- ^ Header organization, embedded in other objects
  , species :: Maybe SpeciesHeader -- ^ Header species, embedded in other objects
  , activeTo :: Maybe ActiveTo -- ^ Ending period when this spacecraft class was in use
  , operator :: Maybe OrganizationHeader -- ^ Header organization, embedded in other objects
  }
  deriving (Eq, Show)

spacecraftClassBaseSchema :: FC.Fleece schema => schema SpacecraftClassBase
spacecraftClassBaseSchema =
  FC.object $
    FC.constructor SpacecraftClassBase
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "activeFrom" activeFrom activeFromSchema
      #+ FC.optional "warpCapable" warpCapable warpCapableSchema
      #+ FC.optional "numberOfDecks" numberOfDecks numberOfDecksSchema
      #+ FC.optional "affiliation" affiliation organizationHeaderSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "owner" owner organizationHeaderSchema
      #+ FC.optional "species" species speciesHeaderSchema
      #+ FC.optional "activeTo" activeTo activeToSchema
      #+ FC.optional "operator" operator organizationHeaderSchema