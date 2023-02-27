{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictFull
  ( ConflictFull(..)
  , conflictFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.LocationBase (LocationBase, locationBaseSchema)
import StarTrek.OrganizationBase (OrganizationBase, organizationBaseSchema)

data ConflictFull = ConflictFull
  { alternateReality :: Maybe Bool -- ^ Whether this conflict is from alternate reality
  , firstSideCommanders :: Maybe [CharacterBase] -- ^ Commanders involved in conflict on first side
  , name :: Text -- ^ Conflict name
  , secondSideCommanders :: Maybe [CharacterBase] -- ^ Commanders involved in conflict on second side
  , yearFrom :: Maybe Integer -- ^ Starting year of the conflict
  , earthConflict :: Maybe Bool -- ^ Whether it is an Earth conflict
  , dominionWarBattle :: Maybe Bool -- ^ Whether this conflict is a Dominion war battle
  , firstSideBelligerents :: Maybe [OrganizationBase] -- ^ Organization involved in conflict on first side
  , uid :: Text -- ^ Conflict unique ID
  , secondSideBelligerents :: Maybe [OrganizationBase] -- ^ Organization involved in conflict on second side
  , klingonWar :: Maybe Bool -- ^ Whether this conflict is a part of war involving the Klingons
  , federationWar :: Maybe Bool -- ^ Whether this conflict is a part of war involving Federation
  , yearTo :: Maybe Integer -- ^ Ending year of the conflict
  , locations :: Maybe [LocationBase] -- ^ Locations this conflict occurred at
  }
  deriving (Eq, Show)

conflictFullSchema :: FC.Fleece schema => schema ConflictFull
conflictFullSchema =
  FC.object $
    FC.constructor ConflictFull
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.optional "firstSideCommanders" firstSideCommanders (FC.list characterBaseSchema)
      #+ FC.required "name" name FC.text
      #+ FC.optional "secondSideCommanders" secondSideCommanders (FC.list characterBaseSchema)
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "earthConflict" earthConflict FC.boolean
      #+ FC.optional "dominionWarBattle" dominionWarBattle FC.boolean
      #+ FC.optional "firstSideBelligerents" firstSideBelligerents (FC.list organizationBaseSchema)
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "secondSideBelligerents" secondSideBelligerents (FC.list organizationBaseSchema)
      #+ FC.optional "klingonWar" klingonWar FC.boolean
      #+ FC.optional "federationWar" federationWar FC.boolean
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "locations" locations (FC.list locationBaseSchema)