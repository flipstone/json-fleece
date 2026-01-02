{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialFull.Drug
  ( Drug(..)
  , drugSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Drug = Drug Bool
  deriving (Show, Eq)

drugSchema :: FC.Fleece t => FC.Schema t Drug
drugSchema =
  FC.coerceSchema FC.boolean