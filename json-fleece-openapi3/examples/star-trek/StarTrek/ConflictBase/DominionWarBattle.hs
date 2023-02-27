{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictBase.DominionWarBattle
  ( DominionWarBattle(..)
  , dominionWarBattleSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype DominionWarBattle = DominionWarBattle Bool
  deriving (Show, Eq)

dominionWarBattleSchema :: FC.Fleece schema => schema DominionWarBattle
dominionWarBattleSchema =
  FC.coerceSchema FC.boolean