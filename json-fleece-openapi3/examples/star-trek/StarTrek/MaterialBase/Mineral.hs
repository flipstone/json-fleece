{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialBase.Mineral
  ( Mineral(..)
  , mineralSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Mineral = Mineral Bool
  deriving (Show, Eq)

mineralSchema :: FC.Fleece schema => schema Mineral
mineralSchema =
  FC.coerceSchema FC.boolean