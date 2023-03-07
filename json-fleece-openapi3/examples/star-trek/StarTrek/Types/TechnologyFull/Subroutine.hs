{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.Subroutine
  ( Subroutine(..)
  , subroutineSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Subroutine = Subroutine Bool
  deriving (Show, Eq)

subroutineSchema :: FC.Fleece schema => schema Subroutine
subroutineSchema =
  FC.coerceSchema FC.boolean