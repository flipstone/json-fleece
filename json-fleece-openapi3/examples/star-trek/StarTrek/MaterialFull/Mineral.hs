{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialFull.Mineral
  ( Mineral(..)
  , mineralSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Mineral = Mineral Bool
  deriving (Show, Eq)

mineralSchema :: FC.Fleece schema => schema Mineral
mineralSchema =
  FC.coerceSchema FC.boolean