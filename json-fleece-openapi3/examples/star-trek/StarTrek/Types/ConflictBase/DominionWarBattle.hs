{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictBase.DominionWarBattle
  ( DominionWarBattle(..)
  , dominionWarBattleSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype DominionWarBattle = DominionWarBattle Bool
  deriving (Show, Eq)

dominionWarBattleSchema :: FC.Fleece t => FC.Schema t DominionWarBattle
dominionWarBattleSchema =
  FC.coerceSchema FC.boolean