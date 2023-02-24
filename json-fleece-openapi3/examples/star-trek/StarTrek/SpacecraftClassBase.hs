{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassBase
  ( SpacecraftClassBase(..)
  , spacecraftClassBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.OrganizationHeader (OrganizationHeader, organizationHeaderSchema)
import StarTrek.SpeciesHeader (SpeciesHeader, speciesHeaderSchema)

data SpacecraftClassBase = SpacecraftClassBase
  { alternateReality :: Maybe Bool -- ^ Whether this spacecraft class is from alternate reality
  , name :: Text -- ^ Spacecraft class name
  , activeFrom :: Maybe Text -- ^ Starting period when this spacecraft class was in use
  , warpCapable :: Maybe Bool -- ^ Whether it's a warp-capable spacecraft class
  , numberOfDecks :: Maybe Integer -- ^ Number of decks
  , affiliation :: Maybe OrganizationHeader -- ^ Header organization, embedded in other objects
  , uid :: Text -- ^ Spacecraft class unique ID
  , owner :: Maybe OrganizationHeader -- ^ Header organization, embedded in other objects
  , species :: Maybe SpeciesHeader -- ^ Header species, embedded in other objects
  , activeTo :: Maybe Text -- ^ Ending period when this spacecraft class was in use
  , operator :: Maybe OrganizationHeader -- ^ Header organization, embedded in other objects
  }
  deriving (Eq, Show)

spacecraftClassBaseSchema :: FC.Fleece schema => schema SpacecraftClassBase
spacecraftClassBaseSchema =
  FC.object $
    FC.constructor SpacecraftClassBase
      #+ FC.optionalField FC.OmitKey_DelegateNull "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "activeFrom" activeFrom FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "warpCapable" warpCapable FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfDecks" numberOfDecks FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "affiliation" affiliation organizationHeaderSchema
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "owner" owner organizationHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "species" species speciesHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "activeTo" activeTo FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "operator" operator organizationHeaderSchema