{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassBase
  ( SpacecraftClassBase(..)
  , spacecraftClassBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
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
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "activeFrom" activeFrom FC.text
      #+ FC.optional "warpCapable" warpCapable FC.boolean
      #+ FC.optional "numberOfDecks" numberOfDecks FC.integer
      #+ FC.optional "affiliation" affiliation organizationHeaderSchema
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "owner" owner organizationHeaderSchema
      #+ FC.optional "species" species speciesHeaderSchema
      #+ FC.optional "activeTo" activeTo FC.text
      #+ FC.optional "operator" operator organizationHeaderSchema