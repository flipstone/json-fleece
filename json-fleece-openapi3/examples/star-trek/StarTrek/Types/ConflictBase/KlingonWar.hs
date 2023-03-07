{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictBase.KlingonWar
  ( KlingonWar(..)
  , klingonWarSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype KlingonWar = KlingonWar Bool
  deriving (Show, Eq)

klingonWarSchema :: FC.Fleece schema => schema KlingonWar
klingonWarSchema =
  FC.coerceSchema FC.boolean