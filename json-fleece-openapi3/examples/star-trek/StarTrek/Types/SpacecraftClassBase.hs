{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassBase
  ( SpacecraftClassBase(..)
  , spacecraftClassBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.OrganizationHeader as OrganizationHeader
import qualified StarTrek.Types.SpacecraftClassBase.ActiveFrom as ActiveFrom
import qualified StarTrek.Types.SpacecraftClassBase.ActiveTo as ActiveTo
import qualified StarTrek.Types.SpacecraftClassBase.AlternateReality as AlternateReality
import qualified StarTrek.Types.SpacecraftClassBase.Name as Name
import qualified StarTrek.Types.SpacecraftClassBase.NumberOfDecks as NumberOfDecks
import qualified StarTrek.Types.SpacecraftClassBase.Uid as Uid
import qualified StarTrek.Types.SpacecraftClassBase.WarpCapable as WarpCapable
import qualified StarTrek.Types.SpeciesHeader as SpeciesHeader

data SpacecraftClassBase = SpacecraftClassBase
  { activeFrom :: Maybe ActiveFrom.ActiveFrom -- ^ Starting period when this spacecraft class was in use
  , activeTo :: Maybe ActiveTo.ActiveTo -- ^ Ending period when this spacecraft class was in use
  , affiliation :: Maybe OrganizationHeader.OrganizationHeader -- ^ Header organization, embedded in other objects
  , alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this spacecraft class is from alternate reality
  , name :: Name.Name -- ^ Spacecraft class name
  , numberOfDecks :: Maybe NumberOfDecks.NumberOfDecks -- ^ Number of decks
  , operator :: Maybe OrganizationHeader.OrganizationHeader -- ^ Header organization, embedded in other objects
  , owner :: Maybe OrganizationHeader.OrganizationHeader -- ^ Header organization, embedded in other objects
  , species :: Maybe SpeciesHeader.SpeciesHeader -- ^ Header species, embedded in other objects
  , uid :: Uid.Uid -- ^ Spacecraft class unique ID
  , warpCapable :: Maybe WarpCapable.WarpCapable -- ^ Whether it's a warp-capable spacecraft class
  }
  deriving (Eq, Show)

spacecraftClassBaseSchema :: FC.Fleece schema => schema SpacecraftClassBase
spacecraftClassBaseSchema =
  FC.object $
    FC.constructor SpacecraftClassBase
      #+ FC.optional "activeFrom" activeFrom ActiveFrom.activeFromSchema
      #+ FC.optional "activeTo" activeTo ActiveTo.activeToSchema
      #+ FC.optional "affiliation" affiliation OrganizationHeader.organizationHeaderSchema
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "numberOfDecks" numberOfDecks NumberOfDecks.numberOfDecksSchema
      #+ FC.optional "operator" operator OrganizationHeader.organizationHeaderSchema
      #+ FC.optional "owner" owner OrganizationHeader.organizationHeaderSchema
      #+ FC.optional "species" species SpeciesHeader.speciesHeaderSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "warpCapable" warpCapable WarpCapable.warpCapableSchema