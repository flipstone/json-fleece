{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialBase.BiochemicalCompound
  ( BiochemicalCompound(..)
  , biochemicalCompoundSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BiochemicalCompound = BiochemicalCompound Bool
  deriving (Show, Eq)

biochemicalCompoundSchema :: FC.Fleece t => FC.Schema t BiochemicalCompound
biochemicalCompoundSchema =
  FC.coerceSchema FC.boolean