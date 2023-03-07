{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassFull
  ( SpacecraftClassFull(..)
  , spacecraftClassFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.OrganizationBase as OrganizationBase
import qualified StarTrek.Types.SpacecraftBase as SpacecraftBase
import qualified StarTrek.Types.SpacecraftClassFull.ActiveFrom as ActiveFrom
import qualified StarTrek.Types.SpacecraftClassFull.ActiveTo as ActiveTo
import qualified StarTrek.Types.SpacecraftClassFull.AlternateReality as AlternateReality
import qualified StarTrek.Types.SpacecraftClassFull.Name as Name
import qualified StarTrek.Types.SpacecraftClassFull.NumberOfDecks as NumberOfDecks
import qualified StarTrek.Types.SpacecraftClassFull.Uid as Uid
import qualified StarTrek.Types.SpacecraftClassFull.WarpCapable as WarpCapable
import qualified StarTrek.Types.SpacecraftType as SpacecraftType
import qualified StarTrek.Types.SpeciesHeader as SpeciesHeader

data SpacecraftClassFull = SpacecraftClassFull
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this spacecraft class is from alternate reality
  , name :: Name.Name -- ^ Spacecraft class name
  , spacecrafts :: Maybe [SpacecraftBase.SpacecraftBase] -- ^ Base spacecraft, returned in search results
  , activeFrom :: Maybe ActiveFrom.ActiveFrom -- ^ Starting period when this spacecraft class was in use
  , warpCapable :: Maybe WarpCapable.WarpCapable -- ^ Whether it's a warp-capable spacecraft class
  , numberOfDecks :: Maybe NumberOfDecks.NumberOfDecks -- ^ Number of decks
  , affiliation :: Maybe OrganizationBase.OrganizationBase -- ^ Base organization, returned in search results
  , uid :: Uid.Uid -- ^ Spacecraft class unique ID
  , owner :: Maybe OrganizationBase.OrganizationBase -- ^ Base organization, returned in search results
  , species :: Maybe SpeciesHeader.SpeciesHeader -- ^ Header species, embedded in other objects
  , activeTo :: Maybe ActiveTo.ActiveTo -- ^ Ending period when this spacecraft class was in use
  , operator :: Maybe OrganizationBase.OrganizationBase -- ^ Base organization, returned in search results
  , spacecraftTypes :: Maybe [SpacecraftType.SpacecraftType] -- ^ Rating of video release, etc.
  }
  deriving (Eq, Show)

spacecraftClassFullSchema :: FC.Fleece schema => schema SpacecraftClassFull
spacecraftClassFullSchema =
  FC.object $
    FC.constructor SpacecraftClassFull
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "spacecrafts" spacecrafts (FC.list SpacecraftBase.spacecraftBaseSchema)
      #+ FC.optional "activeFrom" activeFrom ActiveFrom.activeFromSchema
      #+ FC.optional "warpCapable" warpCapable WarpCapable.warpCapableSchema
      #+ FC.optional "numberOfDecks" numberOfDecks NumberOfDecks.numberOfDecksSchema
      #+ FC.optional "affiliation" affiliation OrganizationBase.organizationBaseSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "owner" owner OrganizationBase.organizationBaseSchema
      #+ FC.optional "species" species SpeciesHeader.speciesHeaderSchema
      #+ FC.optional "activeTo" activeTo ActiveTo.activeToSchema
      #+ FC.optional "operator" operator OrganizationBase.organizationBaseSchema
      #+ FC.optional "spacecraftTypes" spacecraftTypes (FC.list SpacecraftType.spacecraftTypeSchema)