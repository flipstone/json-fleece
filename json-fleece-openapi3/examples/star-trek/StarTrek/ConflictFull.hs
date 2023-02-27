{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictFull
  ( ConflictFull(..)
  , conflictFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ConflictFull.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.ConflictFull.DominionWarBattle (DominionWarBattle, dominionWarBattleSchema)
import StarTrek.ConflictFull.EarthConflict (EarthConflict, earthConflictSchema)
import StarTrek.ConflictFull.FederationWar (FederationWar, federationWarSchema)
import StarTrek.ConflictFull.KlingonWar (KlingonWar, klingonWarSchema)
import StarTrek.ConflictFull.Name (Name, nameSchema)
import StarTrek.ConflictFull.Uid (Uid, uidSchema)
import StarTrek.ConflictFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ConflictFull.YearTo (YearTo, yearToSchema)
import StarTrek.LocationBase (LocationBase, locationBaseSchema)
import StarTrek.OrganizationBase (OrganizationBase, organizationBaseSchema)

data ConflictFull = ConflictFull
  { alternateReality :: Maybe AlternateReality -- ^ Whether this conflict is from alternate reality
  , firstSideCommanders :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , name :: Name -- ^ Conflict name
  , secondSideCommanders :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , yearFrom :: Maybe YearFrom -- ^ Starting year of the conflict
  , earthConflict :: Maybe EarthConflict -- ^ Whether it is an Earth conflict
  , dominionWarBattle :: Maybe DominionWarBattle -- ^ Whether this conflict is a Dominion war battle
  , firstSideBelligerents :: Maybe [OrganizationBase] -- ^ Base organization, returned in search results
  , uid :: Uid -- ^ Conflict unique ID
  , secondSideBelligerents :: Maybe [OrganizationBase] -- ^ Base organization, returned in search results
  , klingonWar :: Maybe KlingonWar -- ^ Whether this conflict is a part of war involving the Klingons
  , federationWar :: Maybe FederationWar -- ^ Whether this conflict is a part of war involving Federation
  , yearTo :: Maybe YearTo -- ^ Ending year of the conflict
  , locations :: Maybe [LocationBase] -- ^ Base location, returned in search results
  }
  deriving (Eq, Show)

conflictFullSchema :: FC.Fleece schema => schema ConflictFull
conflictFullSchema =
  FC.object $
    FC.constructor ConflictFull
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.optional "firstSideCommanders" firstSideCommanders (FC.list characterBaseSchema)
      #+ FC.required "name" name nameSchema
      #+ FC.optional "secondSideCommanders" secondSideCommanders (FC.list characterBaseSchema)
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "earthConflict" earthConflict earthConflictSchema
      #+ FC.optional "dominionWarBattle" dominionWarBattle dominionWarBattleSchema
      #+ FC.optional "firstSideBelligerents" firstSideBelligerents (FC.list organizationBaseSchema)
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "secondSideBelligerents" secondSideBelligerents (FC.list organizationBaseSchema)
      #+ FC.optional "klingonWar" klingonWar klingonWarSchema
      #+ FC.optional "federationWar" federationWar federationWarSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "locations" locations (FC.list locationBaseSchema)