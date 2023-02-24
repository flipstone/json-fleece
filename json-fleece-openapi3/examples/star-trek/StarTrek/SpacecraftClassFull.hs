{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassFull
  ( SpacecraftClassFull(..)
  , spacecraftClassFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "spacecrafts" spacecrafts (FC.list spacecraftBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "activeFrom" activeFrom FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "warpCapable" warpCapable FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfDecks" numberOfDecks FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "affiliation" affiliation organizationBaseSchema
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "owner" owner organizationBaseSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "species" species speciesHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "activeTo" activeTo FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "operator" operator organizationBaseSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "spacecraftTypes" spacecraftTypes (FC.list spacecraftTypeSchema)