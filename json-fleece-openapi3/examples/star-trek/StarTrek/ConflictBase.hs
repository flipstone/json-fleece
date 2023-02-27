{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictBase
  ( ConflictBase(..)
  , conflictBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Integer, Maybe, Show)

data ConflictBase = ConflictBase
  { alternateReality :: Maybe Bool -- ^ Whether this conflict is from alternate reality
  , name :: Text -- ^ Conflict name
  , yearFrom :: Maybe Integer -- ^ Starting year of the conflict
  , earthConflict :: Maybe Bool -- ^ Whether it was an Earth conflict
  , dominionWarBattle :: Maybe Bool -- ^ Whether this conflict is a Dominion war battle
  , uid :: Text -- ^ Conflict unique ID
  , klingonWar :: Maybe Bool -- ^ Whether this conflict is part of war involving the Klingons
  , federationWar :: Maybe Bool -- ^ Whether this conflict is part of war involving Federation
  , yearTo :: Maybe Integer -- ^ Ending year of the conflict
  }
  deriving (Eq, Show)

conflictBaseSchema :: FC.Fleece schema => schema ConflictBase
conflictBaseSchema =
  FC.object $
    FC.constructor ConflictBase
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "earthConflict" earthConflict FC.boolean
      #+ FC.optional "dominionWarBattle" dominionWarBattle FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "klingonWar" klingonWar FC.boolean
      #+ FC.optional "federationWar" federationWar FC.boolean
      #+ FC.optional "yearTo" yearTo FC.integer