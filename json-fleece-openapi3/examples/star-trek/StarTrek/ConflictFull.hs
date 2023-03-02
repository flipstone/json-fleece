{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictFull
  ( ConflictFull(..)
  , conflictFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CharacterBase as CharacterBase
import qualified StarTrek.ConflictFull.AlternateReality as AlternateReality
import qualified StarTrek.ConflictFull.DominionWarBattle as DominionWarBattle
import qualified StarTrek.ConflictFull.EarthConflict as EarthConflict
import qualified StarTrek.ConflictFull.FederationWar as FederationWar
import qualified StarTrek.ConflictFull.KlingonWar as KlingonWar
import qualified StarTrek.ConflictFull.Name as Name
import qualified StarTrek.ConflictFull.Uid as Uid
import qualified StarTrek.ConflictFull.YearFrom as YearFrom
import qualified StarTrek.ConflictFull.YearTo as YearTo
import qualified StarTrek.LocationBase as LocationBase
import qualified StarTrek.OrganizationBase as OrganizationBase

data ConflictFull = ConflictFull
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this conflict is from alternate reality
  , firstSideCommanders :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , name :: Name.Name -- ^ Conflict name
  , secondSideCommanders :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of the conflict
  , earthConflict :: Maybe EarthConflict.EarthConflict -- ^ Whether it is an Earth conflict
  , dominionWarBattle :: Maybe DominionWarBattle.DominionWarBattle -- ^ Whether this conflict is a Dominion war battle
  , firstSideBelligerents :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  , uid :: Uid.Uid -- ^ Conflict unique ID
  , secondSideBelligerents :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  , klingonWar :: Maybe KlingonWar.KlingonWar -- ^ Whether this conflict is a part of war involving the Klingons
  , federationWar :: Maybe FederationWar.FederationWar -- ^ Whether this conflict is a part of war involving Federation
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of the conflict
  , locations :: Maybe [LocationBase.LocationBase] -- ^ Base location, returned in search results
  }
  deriving (Eq, Show)

conflictFullSchema :: FC.Fleece schema => schema ConflictFull
conflictFullSchema =
  FC.object $
    FC.constructor ConflictFull
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "firstSideCommanders" firstSideCommanders (FC.list CharacterBase.characterBaseSchema)
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "secondSideCommanders" secondSideCommanders (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "earthConflict" earthConflict EarthConflict.earthConflictSchema
      #+ FC.optional "dominionWarBattle" dominionWarBattle DominionWarBattle.dominionWarBattleSchema
      #+ FC.optional "firstSideBelligerents" firstSideBelligerents (FC.list OrganizationBase.organizationBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "secondSideBelligerents" secondSideBelligerents (FC.list OrganizationBase.organizationBaseSchema)
      #+ FC.optional "klingonWar" klingonWar KlingonWar.klingonWarSchema
      #+ FC.optional "federationWar" federationWar FederationWar.federationWarSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "locations" locations (FC.list LocationBase.locationBaseSchema)