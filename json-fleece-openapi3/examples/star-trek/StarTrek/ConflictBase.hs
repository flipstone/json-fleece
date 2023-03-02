{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictBase
  ( ConflictBase(..)
  , conflictBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ConflictBase.AlternateReality as AlternateReality
import qualified StarTrek.ConflictBase.DominionWarBattle as DominionWarBattle
import qualified StarTrek.ConflictBase.EarthConflict as EarthConflict
import qualified StarTrek.ConflictBase.FederationWar as FederationWar
import qualified StarTrek.ConflictBase.KlingonWar as KlingonWar
import qualified StarTrek.ConflictBase.Name as Name
import qualified StarTrek.ConflictBase.Uid as Uid
import qualified StarTrek.ConflictBase.YearFrom as YearFrom
import qualified StarTrek.ConflictBase.YearTo as YearTo

data ConflictBase = ConflictBase
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this conflict is from alternate reality
  , name :: Name.Name -- ^ Conflict name
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of the conflict
  , earthConflict :: Maybe EarthConflict.EarthConflict -- ^ Whether it was an Earth conflict
  , dominionWarBattle :: Maybe DominionWarBattle.DominionWarBattle -- ^ Whether this conflict is a Dominion war battle
  , uid :: Uid.Uid -- ^ Conflict unique ID
  , klingonWar :: Maybe KlingonWar.KlingonWar -- ^ Whether this conflict is part of war involving the Klingons
  , federationWar :: Maybe FederationWar.FederationWar -- ^ Whether this conflict is part of war involving Federation
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of the conflict
  }
  deriving (Eq, Show)

conflictBaseSchema :: FC.Fleece schema => schema ConflictBase
conflictBaseSchema =
  FC.object $
    FC.constructor ConflictBase
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "earthConflict" earthConflict EarthConflict.earthConflictSchema
      #+ FC.optional "dominionWarBattle" dominionWarBattle DominionWarBattle.dominionWarBattleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "klingonWar" klingonWar KlingonWar.klingonWarSchema
      #+ FC.optional "federationWar" federationWar FederationWar.federationWarSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema