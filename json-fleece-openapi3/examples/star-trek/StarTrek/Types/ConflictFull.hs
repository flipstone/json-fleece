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
  { secondSideBelligerents :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of the conflict
  , alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this conflict is from alternate reality
  , firstSideCommanders :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , federationWar :: Maybe FederationWar.FederationWar -- ^ Whether this conflict is a part of war involving Federation
  , klingonWar :: Maybe KlingonWar.KlingonWar -- ^ Whether this conflict is a part of war involving the Klingons
  , dominionWarBattle :: Maybe DominionWarBattle.DominionWarBattle -- ^ Whether this conflict is a Dominion war battle
  , uid :: Uid.Uid -- ^ Conflict unique ID
  , locations :: Maybe [LocationBase.LocationBase] -- ^ Base location, returned in search results
  , secondSideCommanders :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , earthConflict :: Maybe EarthConflict.EarthConflict -- ^ Whether it is an Earth conflict
  , firstSideBelligerents :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  , name :: Name.Name -- ^ Conflict name
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of the conflict
  }
  deriving (Eq, Show)

conflictFullSchema :: FC.Fleece schema => schema ConflictFull
conflictFullSchema =
  FC.object $
    FC.constructor ConflictFull
      #+ FC.optional "secondSideBelligerents" secondSideBelligerents (FC.list OrganizationBase.organizationBaseSchema)
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "firstSideCommanders" firstSideCommanders (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "federationWar" federationWar FederationWar.federationWarSchema
      #+ FC.optional "klingonWar" klingonWar KlingonWar.klingonWarSchema
      #+ FC.optional "dominionWarBattle" dominionWarBattle DominionWarBattle.dominionWarBattleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "locations" locations (FC.list LocationBase.locationBaseSchema)
      #+ FC.optional "secondSideCommanders" secondSideCommanders (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "earthConflict" earthConflict EarthConflict.earthConflictSchema
      #+ FC.optional "firstSideBelligerents" firstSideBelligerents (FC.list OrganizationBase.organizationBaseSchema)
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema