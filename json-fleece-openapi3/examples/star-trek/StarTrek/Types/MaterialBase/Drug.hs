{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialBase.Drug
  ( Drug(..)
  , drugSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Drug = Drug Bool
  deriving (Show, Eq)

drugSchema :: FC.Fleece schema => schema Drug
drugSchema =
  FC.coerceSchema FC.boolean