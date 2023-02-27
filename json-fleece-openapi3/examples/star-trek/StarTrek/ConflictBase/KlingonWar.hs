{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictBase.KlingonWar
  ( KlingonWar(..)
  , klingonWarSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype KlingonWar = KlingonWar Bool
  deriving (Show, Eq)

klingonWarSchema :: FC.Fleece schema => schema KlingonWar
klingonWarSchema =
  FC.coerceSchema FC.boolean