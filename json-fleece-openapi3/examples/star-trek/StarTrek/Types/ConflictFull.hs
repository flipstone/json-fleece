{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictFull
  ( ConflictFull(..)
  , conflictFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.ConflictFull.AlternateReality as AlternateReality
import qualified StarTrek.Types.ConflictFull.DominionWarBattle as DominionWarBattle
import qualified StarTrek.Types.ConflictFull.EarthConflict as EarthConflict
import qualified StarTrek.Types.ConflictFull.FederationWar as FederationWar
import qualified StarTrek.Types.ConflictFull.KlingonWar as KlingonWar
import qualified StarTrek.Types.ConflictFull.Name as Name
import qualified StarTrek.Types.ConflictFull.Uid as Uid
import qualified StarTrek.Types.ConflictFull.YearFrom as YearFrom
import qualified StarTrek.Types.ConflictFull.YearTo as YearTo
import qualified StarTrek.Types.LocationBase as LocationBase
import qualified StarTrek.Types.OrganizationBase as OrganizationBase

data ConflictFull = ConflictFull
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this conflict is from alternate reality
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of the conflict
  , secondSideCommanders :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , uid :: Uid.Uid -- ^ Conflict unique ID
  , federationWar :: Maybe FederationWar.FederationWar -- ^ Whether this conflict is a part of war involving Federation
  , earthConflict :: Maybe EarthConflict.EarthConflict -- ^ Whether it is an Earth conflict
  , firstSideCommanders :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , locations :: Maybe [LocationBase.LocationBase] -- ^ Base location, returned in search results
  , firstSideBelligerents :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  , secondSideBelligerents :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  , dominionWarBattle :: Maybe DominionWarBattle.DominionWarBattle -- ^ Whether this conflict is a Dominion war battle
  , klingonWar :: Maybe KlingonWar.KlingonWar -- ^ Whether this conflict is a part of war involving the Klingons
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of the conflict
  , name :: Name.Name -- ^ Conflict name
  }
  deriving (Eq, Show)

conflictFullSchema :: FC.Fleece schema => schema ConflictFull
conflictFullSchema =
  FC.object $
    FC.constructor ConflictFull
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "secondSideCommanders" secondSideCommanders (FC.list CharacterBase.characterBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "federationWar" federationWar FederationWar.federationWarSchema
      #+ FC.optional "earthConflict" earthConflict EarthConflict.earthConflictSchema
      #+ FC.optional "firstSideCommanders" firstSideCommanders (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "locations" locations (FC.list LocationBase.locationBaseSchema)
      #+ FC.optional "firstSideBelligerents" firstSideBelligerents (FC.list OrganizationBase.organizationBaseSchema)
      #+ FC.optional "secondSideBelligerents" secondSideBelligerents (FC.list OrganizationBase.organizationBaseSchema)
      #+ FC.optional "dominionWarBattle" dominionWarBattle DominionWarBattle.dominionWarBattleSchema
      #+ FC.optional "klingonWar" klingonWar KlingonWar.klingonWarSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.required "name" name Name.nameSchema