{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictBase
  ( ConflictBase(..)
  , conflictBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ConflictBase.AlternateReality as AlternateReality
import qualified StarTrek.Types.ConflictBase.DominionWarBattle as DominionWarBattle
import qualified StarTrek.Types.ConflictBase.EarthConflict as EarthConflict
import qualified StarTrek.Types.ConflictBase.FederationWar as FederationWar
import qualified StarTrek.Types.ConflictBase.KlingonWar as KlingonWar
import qualified StarTrek.Types.ConflictBase.Name as Name
import qualified StarTrek.Types.ConflictBase.Uid as Uid
import qualified StarTrek.Types.ConflictBase.YearFrom as YearFrom
import qualified StarTrek.Types.ConflictBase.YearTo as YearTo

data ConflictBase = ConflictBase
  { yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of the conflict
  , alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this conflict is from alternate reality
  , federationWar :: Maybe FederationWar.FederationWar -- ^ Whether this conflict is part of war involving Federation
  , klingonWar :: Maybe KlingonWar.KlingonWar -- ^ Whether this conflict is part of war involving the Klingons
  , dominionWarBattle :: Maybe DominionWarBattle.DominionWarBattle -- ^ Whether this conflict is a Dominion war battle
  , uid :: Uid.Uid -- ^ Conflict unique ID
  , earthConflict :: Maybe EarthConflict.EarthConflict -- ^ Whether it was an Earth conflict
  , name :: Name.Name -- ^ Conflict name
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of the conflict
  }
  deriving (Eq, Show)

conflictBaseSchema :: FC.Fleece schema => schema ConflictBase
conflictBaseSchema =
  FC.object $
    FC.constructor ConflictBase
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "federationWar" federationWar FederationWar.federationWarSchema
      #+ FC.optional "klingonWar" klingonWar KlingonWar.klingonWarSchema
      #+ FC.optional "dominionWarBattle" dominionWarBattle DominionWarBattle.dominionWarBattleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "earthConflict" earthConflict EarthConflict.earthConflictSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema