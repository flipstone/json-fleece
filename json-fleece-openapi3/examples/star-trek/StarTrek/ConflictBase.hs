{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictBase
  ( ConflictBase(..)
  , conflictBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ConflictBase.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.ConflictBase.DominionWarBattle (DominionWarBattle, dominionWarBattleSchema)
import StarTrek.ConflictBase.EarthConflict (EarthConflict, earthConflictSchema)
import StarTrek.ConflictBase.FederationWar (FederationWar, federationWarSchema)
import StarTrek.ConflictBase.KlingonWar (KlingonWar, klingonWarSchema)
import StarTrek.ConflictBase.Name (Name, nameSchema)
import StarTrek.ConflictBase.Uid (Uid, uidSchema)
import StarTrek.ConflictBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ConflictBase.YearTo (YearTo, yearToSchema)

data ConflictBase = ConflictBase
  { alternateReality :: Maybe AlternateReality -- ^ Whether this conflict is from alternate reality
  , name :: Name -- ^ Conflict name
  , yearFrom :: Maybe YearFrom -- ^ Starting year of the conflict
  , earthConflict :: Maybe EarthConflict -- ^ Whether it was an Earth conflict
  , dominionWarBattle :: Maybe DominionWarBattle -- ^ Whether this conflict is a Dominion war battle
  , uid :: Uid -- ^ Conflict unique ID
  , klingonWar :: Maybe KlingonWar -- ^ Whether this conflict is part of war involving the Klingons
  , federationWar :: Maybe FederationWar -- ^ Whether this conflict is part of war involving Federation
  , yearTo :: Maybe YearTo -- ^ Ending year of the conflict
  }
  deriving (Eq, Show)

conflictBaseSchema :: FC.Fleece schema => schema ConflictBase
conflictBaseSchema =
  FC.object $
    FC.constructor ConflictBase
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "earthConflict" earthConflict earthConflictSchema
      #+ FC.optional "dominionWarBattle" dominionWarBattle dominionWarBattleSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "klingonWar" klingonWar klingonWarSchema
      #+ FC.optional "federationWar" federationWar federationWarSchema
      #+ FC.optional "yearTo" yearTo yearToSchema