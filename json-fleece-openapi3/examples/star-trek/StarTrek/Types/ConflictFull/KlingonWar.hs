{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictFull.KlingonWar
  ( KlingonWar(..)
  , klingonWarSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype KlingonWar = KlingonWar Bool
  deriving (Show, Eq)

klingonWarSchema :: FC.Fleece t => FC.Schema t KlingonWar
klingonWarSchema =
  FC.coerceSchema FC.boolean